/**
 * # Perspective Language
 *
 * Expression language for storing/retrieving Perspective objects.
 * Currently a stub — get() and create() are not implemented.
 */

import { defineLanguage } from '@coasys/ad4m-ldk';
import Icon from './build/Icon.js';
import ConstructorIcon from './build/ConstructorIcon.js';

const language = defineLanguage({
    name: "perspective-language",
    version: "0.1.0",

    async init() {},
    async teardown() {},
    interactions() { return []; },

    expression: {
        async get(_address) {
            console.log("PerspectiveLanguage: Sorry language has not been implemented yet!");
            return null;
        },
        async create(_content) {
            console.log("PerspectiveLanguage: Sorry language has not been implemented yet!");
            return "";
        },
        icon() { return Icon; },
        constructorIcon() { return ConstructorIcon; },
    },
});

export const {
    name,
    version,
    init,
    teardown,
    interactions,
    expressionGet,
    expressionCreate,
    expressionIcon,
    expressionConstructorIcon,
} = language;
