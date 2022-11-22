import { homedir } from 'os'
import path from 'path'

export function ad4mDataDirectory(override?: string) {
    if(override)
        return override
    else
        return path.join(homedir(), '.ad4m')
}