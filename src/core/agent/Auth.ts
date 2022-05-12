export const AllPermission = "*";

export const DefaultTokenValidPeriod = 7 * 24 * 60 * 60; // 7 days in seconds

export interface AuthInfoExtended {
    requestId: string,
    auth: AuthInfo, 
}
export interface AuthInfo {
    requestId: string,
    expiredAt: number,
    appName: string,
    appDesc: string,
    appUrl: string,
    capabilities?: string[], 
}

export interface PermitResult {
    isPermitted: boolean,
    rand?: number,
}

export const genAuthRand = () => {
    return Math.floor(100000 + Math.random() * 900000)
}

export const genAuthKey = (requestId: string, rand: number) => {
    return `${requestId}-${rand}`
}