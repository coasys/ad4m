// import { ReactiveController, ReactiveControllerHost } from 'lit';
// import { ConnectionStates, AuthStates } from '../core';

// /**
//  * UI View States
//  */
// export type UIView = 
//   | 'connection_overview'
//   | 'remote_connection'
//   | 'settings'
//   | 'start'
//   | 'qr'
//   | 'requestcap'
//   | 'verifycode'
//   | 'disconnected'
//   | 'hosting'
//   | 'agentlocked'
//   | 'multiuser_auth';

// /**
//  * Multi-user authentication steps
//  */
// export type MultiUserStep = 'email' | 'password' | 'code';
// export type MultiUserVerificationType = 'signup' | 'login';

// /**
//  * Form state for different auth methods
//  */
// interface FormState {
//   // Legacy hosting
//   email: string;
//   password: string;
//   hostingStep: number;
  
//   // Multi-user auth
//   multiUserEmail: string;
//   multiUserPassword: string;
//   multiUserVerificationCode: string;
//   multiUserStep: MultiUserStep;
//   multiUserVerificationType: MultiUserVerificationType;
  
//   // Remote connection
//   remoteUrl: string;
  
//   // Verification code (for regular auth)
//   code: string | null;
// }

// /**
//  * Error state for different operations
//  */
// interface ErrorState {
//   password: string | null;
//   hostingNotRunning: string | null;
//   verifyCode: string | null;
//   multiUser: string | null;
//   remote: string | null;
// }

// /**
//  * Loading state for async operations
//  */
// interface LoadingState {
//   hosting: boolean;
//   multiUser: boolean;
//   remoteDetecting: boolean;
// }

// /**
//  * Detection state
//  */
// interface DetectionState {
//   localDetected: boolean;
//   remoteMultiUserDetected: boolean | null;
// }

// /**
//  * Reactive state controller for Ad4m Connect
//  * Manages all UI and form state in a centralized, reactive way
//  */
// export class ConnectState implements ReactiveController {
//   private host: ReactiveControllerHost;
  
//   // UI State
//   currentView: UIView = 'connection_overview';
//   isOpen: boolean = false;
//   isMobile: boolean = false;
//   isRemote: boolean = false;
//   hasClickedDownload: boolean = false;
  
//   // Form State
//   forms: FormState = {
//     email: '',
//     password: '',
//     hostingStep: 0,
//     multiUserEmail: '',
//     multiUserPassword: '',
//     multiUserVerificationCode: '',
//     multiUserStep: 'email',
//     multiUserVerificationType: 'login',
//     remoteUrl: '',
//     code: null,
//   };
  
//   // Error State
//   errors: ErrorState = {
//     password: null,
//     hostingNotRunning: null,
//     verifyCode: null,
//     multiUser: null,
//     remote: null,
//   };
  
//   // Loading State
//   loading: LoadingState = {
//     hosting: false,
//     multiUser: false,
//     remoteDetecting: false,
//   };
  
//   // Detection State
//   detection: DetectionState = {
//     localDetected: false,
//     remoteMultiUserDetected: null,
//   };
  
//   constructor(host: ReactiveControllerHost) {
//     this.host = host;
//     host.addController(this);
//   }
  
//   hostConnected() {
//     // Called when the host element is connected to the DOM
//   }
  
//   hostDisconnected() {
//     // Called when the host element is disconnected from the DOM
//   }
  
//   // ===== Computed Properties =====
  
//   get canSubmitMultiUser(): boolean {
//     return Boolean(this.forms.multiUserEmail && this.forms.multiUserPassword);
//   }
  
//   get canSubmitVerificationCode(): boolean {
//     return this.forms.multiUserVerificationCode.length === 6;
//   }
  
//   get hasAnyError(): boolean {
//     return Object.values(this.errors).some(error => error !== null);
//   }
  
//   get isAnyLoading(): boolean {
//     return Object.values(this.loading).some(loading => loading);
//   }
  
//   get showLocalOption(): boolean {
//     return !this.isMobile && this.detection.localDetected;
//   }
  
//   get showRemoteOption(): boolean {
//     return true; // Always available
//   }
  
//   get showQROption(): boolean {
//     return this.isMobile;
//   }
  
//   get showDownload(): boolean {
//     return !this.detection.localDetected && !this.detection.remoteMultiUserDetected;
//   }
  
//   // ===== UI Actions =====
  
//   setView(view: UIView) {
//     this.currentView = view;
//     this.requestUpdate();
//   }
  
//   toggleOpen() {
//     this.isOpen = !this.isOpen;
//     this.requestUpdate();
//   }
  
//   open() {
//     this.isOpen = true;
//     this.requestUpdate();
//   }
  
//   close() {
//     this.isOpen = false;
//     this.requestUpdate();
//   }
  
//   setMobile(isMobile: boolean) {
//     this.isMobile = isMobile;
//     this.requestUpdate();
//   }
  
//   setRemote(isRemote: boolean) {
//     this.isRemote = isRemote;
//     this.requestUpdate();
//   }
  
//   setHasClickedDownload(clicked: boolean) {
//     this.hasClickedDownload = clicked;
//     this.requestUpdate();
//   }
  
//   // ===== Form Actions =====
  
//   updateForm<K extends keyof FormState>(field: K, value: FormState[K]) {
//     this.forms[field] = value;
//     this.requestUpdate();
//   }
  
//   resetMultiUserForm() {
//     this.forms.multiUserEmail = '';
//     this.forms.multiUserPassword = '';
//     this.forms.multiUserVerificationCode = '';
//     this.forms.multiUserStep = 'email';
//     this.requestUpdate();
//   }
  
//   nextMultiUserStep() {
//     if (this.forms.multiUserStep === 'email') {
//       this.forms.multiUserStep = 'password';
//     } else if (this.forms.multiUserStep === 'password') {
//       this.forms.multiUserStep = 'code';
//     }
//     this.requestUpdate();
//   }
  
//   previousMultiUserStep() {
//     if (this.forms.multiUserStep === 'code') {
//       this.forms.multiUserStep = 'password';
//     } else if (this.forms.multiUserStep === 'password') {
//       this.forms.multiUserStep = 'email';
//     }
//     this.requestUpdate();
//   }
  
//   toggleMultiUserVerificationType() {
//     this.forms.multiUserVerificationType = 
//       this.forms.multiUserVerificationType === 'login' ? 'signup' : 'login';
//     this.requestUpdate();
//   }
  
//   // ===== Error Actions =====
  
//   setError<K extends keyof ErrorState>(field: K, message: string | null) {
//     this.errors[field] = message;
//     this.requestUpdate();
//   }
  
//   clearError<K extends keyof ErrorState>(field: K) {
//     this.errors[field] = null;
//     this.requestUpdate();
//   }
  
//   clearAllErrors() {
//     this.errors = {
//       password: null,
//       hostingNotRunning: null,
//       verifyCode: null,
//       multiUser: null,
//       remote: null,
//     };
//     this.requestUpdate();
//   }
  
//   // ===== Loading Actions =====
  
//   setLoading<K extends keyof LoadingState>(field: K, isLoading: boolean) {
//     this.loading[field] = isLoading;
//     this.requestUpdate();
//   }
  
//   // ===== Detection Actions =====
  
//   setLocalDetected(detected: boolean) {
//     this.detection.localDetected = detected;
//     this.requestUpdate();
//   }
  
//   setRemoteMultiUserDetected(detected: boolean | null) {
//     this.detection.remoteMultiUserDetected = detected;
//     this.requestUpdate();
//   }
  
//   // ===== Helper Methods =====
  
//   private requestUpdate() {
//     this.host.requestUpdate();
//   }
  
//   /**
//    * Reset all state to initial values
//    */
//   reset() {
//     this.currentView = 'connection_overview';
//     this.isOpen = false;
//     this.isRemote = false;
//     this.hasClickedDownload = false;
    
//     this.forms = {
//       email: '',
//       password: '',
//       hostingStep: 0,
//       multiUserEmail: '',
//       multiUserPassword: '',
//       multiUserVerificationCode: '',
//       multiUserStep: 'email',
//       multiUserVerificationType: 'login',
//       remoteUrl: '',
//       code: null,
//     };
    
//     this.clearAllErrors();
    
//     this.loading = {
//       hosting: false,
//       multiUser: false,
//       remoteDetecting: false,
//     };
    
//     this.detection = {
//       localDetected: false,
//       remoteMultiUserDetected: null,
//     };
    
//     this.requestUpdate();
//   }
// }
