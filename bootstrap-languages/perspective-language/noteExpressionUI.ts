import type { ExpressionUI } from "@perspect3vism/ad4m";
import Icon from './build/Icon.js'
import ConstructorIcon from './build/ConstructorIcon.js'

export class NoteExpressionUI implements ExpressionUI {
    icon(): string {
        return Icon
    }

    constructorIcon(): string {
        return ConstructorIcon
    }
}