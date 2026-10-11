// The program editor: CodeMirror 6 with a mode for Troupe. The mode follows
// compiler/src/Lexer.x rule for rule -- its keyword list; comments, which
// nest, and the (*) line comment; strings, in which a backslash takes the
// character after it; `{...}` labels, and `< ... >` DC labels with the
// lexer's own state for their inside; the numeric literal forms; type
// variables; and the operator rule, a maximal run of operator characters
// beginning with an initial one, under which a reserved spelling keeps its
// own token. What the lexer rejects is marked invalid.
//
// The colours are the page's: the highlight style names the --syn-*
// custom properties that style.css defines for both themes, and the chrome
// uses the page's --panel, --ink and the rest, so a theme change needs no
// reconfiguration.

import { EditorState, Prec } from '@codemirror/state';
import { EditorView, drawSelection, highlightActiveLineGutter, keymap, lineNumbers } from '@codemirror/view';
import { HighlightStyle, StreamLanguage, bracketMatching, indentUnit, syntaxHighlighting } from '@codemirror/language';
import { defaultKeymap, history, historyKeymap, indentLess, indentMore } from '@codemirror/commands';
import { tags as t } from '@lezer/highlight';

// ---- the mode -------------------------------------------------------------

// Lexer.x, the <0> rules with a word on the left.
const KEYWORDS = new Set(['let', 'in', 'end', 'val', 'fun', 'and', 'if', 'then', 'else', 'fn', 'hn',
  'case', 'of', 'pini', 'when', 'with', 'datatype', 'infixl', 'infixr', 'infix']);
const MODULE_KEYWORDS = new Set(['import', 'require', 'native', 'qualified', 'as']);
const OPERATOR_KEYWORDS = new Set(['andalso', 'orelse', 'raisedTo', 'isTuple', 'isList', 'isRecord',
  'not', 'div', 'mod', 'andb', 'orb', 'xorb']);
const BOOLS = new Set(['true', 'false']);

const SYM = /^[A-Za-z_][A-Za-z0-9_']*/;            // @sym
const TYVAR = /^'[A-Za-z][A-Za-z0-9_']*/;          // @tyvar
const STRING = /^"(?:[^"\\\n]|\\[^\n])*"/;         // @string: \ takes the next character
const LABEL = /^`\{[^}\n]*\}`/;                     // @label
const DC_OPEN = /^`\s*</;                          // TokenDCLabelLeft, enters state_dclabel
const DC_CLOSE = /^>\s*`/;                         // TokenDCLabelRight, leaves it
const DC_CONST = /^#(?:root|null)-(?:confidentiality|integrity)/;
const FLOAT = /^\d[\d_]*\.\d[\d_]*(?:[eE][+-]?\d[\d_]*)?/;
const INT = /^(?:0[bB][01][01_]*|0[oO][0-7][0-7_]*|0[xX][0-9A-Fa-f][0-9A-Fa-f_]*|\d[\d_]*n?)/;
// $opinitial $opchar*: the initials exclude ! ~ ? : . which may continue a run.
const OPERATOR = /^[$%&*+\-/<=>@^|][$%&*+\-/<=>@^|!~?:.]*/;

function blockComment(stream, state) {
  while (!stream.eol()) {
    if (stream.match('(*')) state.comment++;
    else if (stream.match('*)')) { if (--state.comment === 0) return 'blockComment'; }
    else stream.next();
  }
  return 'blockComment';
}

// Inside `< ... >`: principals, the four #root/#null constants, ; | & ( ).
function dcLabel(stream, state) {
  if (stream.eatSpace()) return null;
  if (stream.match(DC_CLOSE)) { state.dcLabel = false; return 'labelName'; }
  if (stream.match(DC_CONST) || stream.match(SYM)) return 'labelName';
  if (stream.match(/^[;()|&]/)) return 'punctuation';
  stream.next();
  return 'invalid';
}

function token(stream, state) {
  if (state.comment > 0) return blockComment(stream, state);
  if (state.dcLabel) return dcLabel(stream, state);
  if (stream.eatSpace()) return null;
  // "(*)" to the end of the line is a comment; "(*" opens one that nests.
  if (stream.match('(*)')) { stream.skipToEnd(); return 'lineComment'; }
  if (stream.match('(*')) { state.comment = 1; return blockComment(stream, state); }
  if (stream.match(STRING)) return 'string';
  if (stream.peek() === '"') { stream.skipToEnd(); return 'string'; }   // not closed on this line
  if (stream.match(LABEL)) return 'labelName';
  if (stream.match(DC_OPEN)) { state.dcLabel = true; return 'labelName'; }
  if (stream.match('#true') || stream.match('#false')) return 'labelName';
  if (stream.match(FLOAT)) return 'number';
  if (stream.match(INT)) return stream.match(SYM) ? 'invalid' : 'number';   // "Invalid literal 12ab"
  if (stream.match(TYVAR)) return 'typeName';
  const sym = stream.match(SYM);
  if (sym) {
    const word = sym[0];
    if (word === '_') return 'punctuation';                 // TokenWildcard
    if (KEYWORDS.has(word)) return 'keyword';
    if (MODULE_KEYWORDS.has(word)) return 'moduleKeyword';
    if (OPERATOR_KEYWORDS.has(word)) return 'operatorKeyword';
    if (BOOLS.has(word)) return 'bool';
    return 'variableName';
  }
  if (stream.match('~>>')) return 'operator';
  if (stream.match(OPERATOR)) return stream.current() === '|' ? 'punctuation' : 'operator';
  if (stream.match(/^(?:::?|\.\.?|[;,])/)) return 'punctuation';
  if (stream.match(/^[()]/)) return 'paren';
  if (stream.match(/^[[\]]/)) return 'squareBracket';
  if (stream.match(/^[{}]/)) return 'brace';
  stream.next();
  return 'invalid';
}

export const troupeLanguage = StreamLanguage.define({
  name: 'troupe',
  startState: () => ({ comment: 0, dcLabel: false }),
  token,
  languageData: { commentTokens: { block: { open: '(*', close: '*)' } } },
});

// ---- the look -------------------------------------------------------------

// Hue, weight and slant together: keywords are the page's blue and bold,
// strings orange, numbers and labels violet (labels bold), comments grey and
// italic; what the lexer rejects is dotted-underlined, not coloured.
const highlight = HighlightStyle.define([
  { tag: [t.keyword, t.moduleKeyword], color: 'var(--syn-keyword)', fontWeight: '600' },
  { tag: t.operatorKeyword, color: 'var(--syn-keyword)' },
  { tag: [t.number, t.bool], color: 'var(--syn-constant)' },
  { tag: t.typeName, color: 'var(--syn-constant)', fontStyle: 'italic' },
  { tag: t.string, color: 'var(--syn-string)' },
  { tag: t.labelName, color: 'var(--syn-label)', fontWeight: '600' },
  { tag: [t.lineComment, t.blockComment], color: 'var(--syn-comment)', fontStyle: 'italic' },
  { tag: [t.operator, t.punctuation, t.paren, t.squareBracket, t.brace], color: 'var(--syn-operator)' },
  { tag: t.invalid, textDecoration: 'underline dotted', textUnderlineOffset: '3px' },
]);

const theme = EditorView.theme({
  '&': { backgroundColor: 'var(--panel)', color: 'var(--ink)' },
  '&.cm-focused': { outline: 'none' },
  '.cm-scroller': { fontFamily: 'var(--mono)', fontSize: '13px', lineHeight: '1.5', overflow: 'auto' },
  '.cm-content': { padding: '12px 0', caretColor: 'var(--accent)' },
  '.cm-line': { padding: '0 14px' },
  '.cm-cursor, .cm-dropCursor': { borderLeftColor: 'var(--accent)' },
  '.cm-selectionBackground, .cm-content ::selection': { backgroundColor: 'var(--term-selection)' },
  '&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground': { backgroundColor: 'var(--term-selection)' },
  '.cm-gutters': { backgroundColor: 'var(--panel-2)', color: 'var(--dim)', borderRight: '1px solid var(--line-2)' },
  '.cm-activeLineGutter': { backgroundColor: 'transparent', color: 'var(--ink-2)' },
  '.cm-lineNumbers .cm-gutterElement': { padding: '0 8px 0 12px', minWidth: '36px' },
  '&.cm-focused .cm-matchingBracket': { backgroundColor: 'transparent', outline: '1px solid var(--accent)', borderRadius: '2px' },
  '&.cm-focused .cm-nonmatchingBracket': { backgroundColor: 'transparent', textDecoration: 'underline dotted' },
});

// ---- the keys -------------------------------------------------------------

// Tab inserts two spaces at the cursor, as the text area did; with a
// selection it indents the lines, and Shift-Tab dedents them.
function insertTwoSpaces(view) {
  if (view.state.selection.ranges.some((r) => !r.empty)) return indentMore(view);
  view.dispatch(view.state.replaceSelection('  '), { scrollIntoView: true, userEvent: 'input' });
  return true;
}

// ---- the editor -----------------------------------------------------------

/**
 * Mounts an editor in `parent`. `onRun` is called on Ctrl-Enter (Cmd-Enter).
 * The result's `value` is the document's text, readable and assignable.
 */
export function createEditor({ parent, doc = '', onRun = () => {} }) {
  const view = new EditorView({
    parent,
    state: EditorState.create({
      doc,
      extensions: [
        // Mod is Cmd on a Mac and Ctrl elsewhere; Ctrl-Enter runs on a Mac too.
        Prec.high(keymap.of([
          { key: 'Mod-Enter', run: () => { onRun(); return true; } },
          { key: 'Ctrl-Enter', run: () => { onRun(); return true; } },
          { key: 'Tab', run: insertTwoSpaces, shift: indentLess },
        ])),
        lineNumbers(),
        highlightActiveLineGutter(),
        history(),
        drawSelection(),
        bracketMatching(),
        indentUnit.of('  '),
        EditorState.tabSize.of(2),
        troupeLanguage,
        syntaxHighlighting(highlight),
        theme,
        keymap.of([...defaultKeymap, ...historyKeymap]),
      ],
    }),
  });
  return {
    view,
    get value() { return view.state.doc.toString(); },
    set value(text) { view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: text } }); },
    focus() { view.focus(); },
  };
}
