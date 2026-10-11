// The page's theme, as the terminal needs it: xterm.js takes its colours as
// options, not from CSS. The colours themselves live in style.css, as the
// --term-* and --ansi-* custom properties on :root for the theme in force;
// this module reads them back, so the terminal and the page cannot drift
// apart. The table below is the fallback for a page whose stylesheet has not
// loaded, and it repeats the stylesheet's values.

/** The theme in force: the chosen one, or the system's when "auto". */
export function effectiveTheme() {
  const chosen = document.documentElement.dataset.theme;
  if (chosen === 'light' || chosen === 'dark') return chosen;
  return matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
}

// ITheme key -> custom property on :root.
const SLOTS = {
  background: '--term-bg',
  foreground: '--term-fg',
  cursor: '--term-cursor',
  selectionBackground: '--term-selection',
  black: '--ansi-black',
  red: '--ansi-red',
  green: '--ansi-green',
  yellow: '--ansi-yellow',
  blue: '--ansi-blue',
  magenta: '--ansi-magenta',
  cyan: '--ansi-cyan',
  white: '--ansi-white',
  brightBlack: '--ansi-bright-black',
  brightRed: '--ansi-bright-red',
  brightGreen: '--ansi-bright-green',
  brightYellow: '--ansi-bright-yellow',
  brightBlue: '--ansi-bright-blue',
  brightMagenta: '--ansi-bright-magenta',
  brightCyan: '--ansi-bright-cyan',
  brightWhite: '--ansi-bright-white',
};

// The same values as style.css, for a page without it. The "red" slot is a
// magenta, not a red: the palette has no red.
const FALLBACK = {
  light: {
    background: '#fafbfc', foreground: '#1b1d22', cursor: '#0072b2', selectionBackground: '#0072b240',
    black: '#1b1d22', red: '#9f2b86', green: '#00704f', yellow: '#9a6500',
    blue: '#0072b2', magenta: '#5b3fb8', cyan: '#0a7b8a', white: '#7c838e',
    brightBlack: '#5b606b', brightRed: '#b8449c', brightGreen: '#008a62', brightYellow: '#b37a00',
    brightBlue: '#2a8fce', brightMagenta: '#7357d1', brightCyan: '#1a93a3', brightWhite: '#a0a6b0',
  },
  dark: {
    background: '#0e1013', foreground: '#e6e8ec', cursor: '#56b4e9', selectionBackground: '#56b4e950',
    black: '#2d323b', red: '#d98cc0', green: '#3fbf96', yellow: '#f0cf5a',
    blue: '#56b4e9', magenta: '#a995ee', cyan: '#5fd0dc', white: '#c4c8d0',
    brightBlack: '#6b7280', brightRed: '#eba8d3', brightGreen: '#6fd9b3', brightYellow: '#f6df85',
    brightBlue: '#8fcff3', brightMagenta: '#c3b4f6', brightCyan: '#8fe3eb', brightWhite: '#f2f3f5',
  },
};

/** xterm.js colours for the theme in force. */
export function terminalTheme() {
  const fallback = FALLBACK[effectiveTheme()];
  const style = getComputedStyle(document.documentElement);
  const theme = {};
  for (const [key, property] of Object.entries(SLOTS)) {
    const value = style.getPropertyValue(property).trim();
    theme[key] = value || fallback[key];
  }
  return theme;
}
