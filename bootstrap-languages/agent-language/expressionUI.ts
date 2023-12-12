import type { ExpressionUI } from "@coasys/ad4m";
import Icon from './build/Icon.js'
import ConstructorIcon from './build/ConstructorIcon.js'

export class UI implements ExpressionUI {
    icon(): string {
        return Icon
    }

    constructorIcon(): string {
        return ConstructorIcon
    }
}