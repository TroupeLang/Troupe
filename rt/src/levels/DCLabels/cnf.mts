import { CONJ_OPERATOR
       , CAT_DELIM_LEFT
       , CAT_DELIM_RIGHT
       , DISJ_OPERATOR
       , Delimiterification
    } from './dcl_pp_config.mjs'

import { Label, LabelJSON, RegularLabel, labelFromJSON, labelImplies } from './label.mjs'

/**
 * A Category represents a disjunction of labels (a clause in CNF).
 * Uses a Map for efficient key-based lookup while preserving Label objects.
 */
export class Category {
    private _labels: Map<string, Label>  // key -> Label
    // The label array, materialized once: a Category is immutable after
    // construction, so this is a derived field, not a cache. Entailment checks
    // iterate labels on the hot path; re-materializing from the Map per call
    // was a measured cost. Consumers never mutate the returned array.
    private _labelsArray: Label[]

    constructor(labels: Label[]) {
        this._labels = new Map();
        for (const label of labels) {
            this._labels.set(label.toKey(), label);
        }
        this._labelsArray = Array.from(this._labels.values());
    }

    /**
     * Returns the number of labels in this category.
     */
    get size(): number {
        return this._labels.size;
    }

    /**
     * Returns true if this category has no labels (represents FALSE in a clause).
     */
    isEmpty(): boolean {
        return this._labels.size === 0;
    }

    /**
     * Returns all labels as an array.
     */
    getLabels(): Label[] {
        return this._labelsArray;
    }

    /**
     * Checks if this category contains a label with the given key.
     */
    hasKey(key: string): boolean {
        return this._labels.has(key);
    }

    /**
     * Creates the union of this category with another.
     */
    union(other: Category): Category {
        const combined: Label[] = [...this._labels.values()];
        for (const label of other._labels.values()) {
            if (!this._labels.has(label.toKey())) {
                combined.push(label);
            }
        }
        return new Category(combined);
    }

    /**
     * String representation for display.
     */
    stringRep(pp_empty_cat: string): string {
        if (this.isEmpty()) {
            return pp_empty_cat;
        }
        return this.getLabels()
            .map(l => l.stringRep())
            .sort()
            .join(DISJ_OPERATOR);
    }

    /**
     * JSON serialization.
     */
    toJSON(): LabelJSON[] {
        return this.getLabels().map(l => l.toJSON());
    }

    /**
     * Deserialize from JSON.
     */
    static fromJSON(o: LabelJSON[] | string[]): Category {
        const labels = o.map((item: LabelJSON | string) => {
            if (typeof item === 'string') {
                // Legacy format: plain string -> RegularLabel
                return new RegularLabel(item);
            }
            return labelFromJSON(item as LabelJSON);
        });
        return new Category(labels);
    }

    /**
     * Create a Category from a set of strings (for backward compatibility).
     * @deprecated Use constructor with Label[] instead
     */
    static fromStringSet(strings: Set<string>): Category {
        const labels = Array.from(strings).map(s => new RegularLabel(s));
        return new Category(labels);
    }
}

export class CNF {
    categories: Set<Category>

    constructor(categories: Set<Category> | Category[]) {
        this.categories = categories instanceof Set
            ? categories
            : new Set(categories);
    }

    equals(other: CNF): boolean {
        return implies(this, other) && implies(other, this);
    }

    stringRep(pp_literals: { trueLit: string; falseLit: string }, parenthesize = Delimiterification.AsNeeded): string {
        if (this.categories.size == 0) {
            return pp_literals.trueLit;
        }

        let p: boolean;
        switch (parenthesize) {
            case Delimiterification.AsNeeded:
                p = this.categories.size > 1;
                break;
            case Delimiterification.Always:
                p = true;
                break;
            case Delimiterification.None:
                p = false;
                break;
        }

        const g = (x: Category): string => {
            let s: string = x.stringRep(pp_literals.falseLit);
            let q = p && (x.size > 1);
            if (q) {
                return (CAT_DELIM_LEFT + s + CAT_DELIM_RIGHT);
            } else {
                return s;
            }
        };

        return Array.from(this.categories)
            .map(g)
            .sort((a, b) => a.localeCompare(b))
            .join(CONJ_OPERATOR);
    }

    /**
     * JSON serialization.
     */
    toJSON(): LabelJSON[][] {
        return Array.from(this.categories).map(c => c.toJSON());
    }

    /**
     * Deserialize from JSON.
     */
    static fromJSON(o: (LabelJSON[] | string[])[]): CNF {
        return new CNF(new Set(o.map(c => Category.fromJSON(c))));
    }
}

/**
 * Check if clause x implies clause y.
 *
 * x implies y if for every label in x, there exists a label in y that it implies.
 * This supports the qfalse implication rule: qfalse@node:tag implies any
 * quarantined label with the same node:tag.
 */
function clauseImplies(x: Category, y: Category): boolean {
    for (const xLabel of x.getLabels()) {
        let found = false;
        for (const yLabel of y.getLabels()) {
            if (labelImplies(xLabel, yLabel)) {
                found = true;
                break;
            }
        }
        if (!found) return false;
    }
    return true;
}

/**
 * Check if CNF X implies CNF Y.
 *
 * For every clause y in Y, there must exist a clause x in X such that x implies y.
 */
export function implies(X: CNF, Y: CNF): boolean {
    for (const y of Y.categories) {
        let found = false;
        for (const x of X.categories) {
            if (clauseImplies(x, y)) {
                found = true;
                break;
            }
        }
        if (!found) return false;
    }
    return true;
}

/**
 * Conjunction of two CNFs (AND).
 *
 * A & B in CNF means taking the union of their clauses.
 */
export function conjunction(X: CNF, Y: CNF): CNF {
    if (implies(X, Y)) {
        return X;
    }
    if (implies(Y, X)) {
        return Y;
    }
    return new CNF(X.categories.union(Y.categories));
}

/**
 * Disjunction of two CNFs (OR).
 *
 * A | B in CNF means taking the cross-product of their clauses.
 */
export function disjunction(X: CNF, Y: CNF): CNF {
    if (implies(X, Y)) {
        return Y;
    }
    if (implies(Y, X)) {
        return X;
    }

    const newCategories: Category[] = [];

    for (const xCat of X.categories) {
        for (const yCat of Y.categories) {
            newCategories.push(xCat.union(yCat));
        }
    }

    return new CNF(new Set(newCategories));
}

/**
 * CNF_TRUE: No clauses - satisfied by everything.
 * Represents the "true" or "public" label.
 */
export const CNF_TRUE: CNF = new CNF(new Set());

/**
 * CNF_FALSE: One empty clause - satisfied by nothing.
 * Represents the "false" or "root authority" label.
 */
export const CNF_FALSE: CNF = new CNF(new Set([new Category([])]));
