import js from "@eslint/js";
import globals from "globals";
import tseslint from "typescript-eslint";
import { defineConfig } from "eslint/config";

export default defineConfig([
  {
    files: ["**/*.{js,mjs,cjs,ts,mts,ct}"],
    languageOptions: {
      parserOptions: {
        projectService: true,
      },
      globals: globals.browser
    },
    plugins: { js },
    extends: ["js/recommended"],
  },
  ...tseslint.configs.recommendedTypeChecked,
  ...tseslint.configs.strictTypeChecked,
  ...tseslint.configs.stylisticTypeChecked,
  {
    rules: {
      // End all lines with a semi colon
      "semi": ["warn", "always"],

      // Do not allow trailing whitespace
      "no-trailing-spaces": ["warn", {
        skipBlankLines: false,
        ignoreComments: false,
      }],

      // Allow `console.log(...)`
      "no-console": "off",

      // TODO: Disable indexed-types vs. `Record<.., ..>` since ESLint confuses
      // them with the runtime records of the same name.
      "@typescript-eslint/consistent-indexed-object-style": "off",

      // TODO: Enabling 'trailing commas' below adds lots of missing commas
      //                 in the wrong places.
      //"comma-dangle":       ["warn", "always"],
      "comma-dangle": "off",
    },
  },
]);
