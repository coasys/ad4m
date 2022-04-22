export const AllPermission = "*";

export const DefaultTokenValidPeriod = 7 * 24 * 60 * 60; // 7 days in seconds

export interface AuthInfo {
    token: string,
    expiredAt: number,
    appName: string,
    appDesc: string,
    appUrl: string,
    permissions?: string[], 
}
