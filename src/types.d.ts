import("./core/PerspectivismCore");  // Don't delete this line.
import { PerspectivismCore } from "./core/PerspectivismCore";

//Module types
export function init(appDataPath: String, resourcePath: string, appDefaultLangPath: string, appDefaultLangs: string[]): Promise<PerspectivismCore>;
export {PerspectivismCore};

//GraphQL interface types
export type Maybe<T> = T | null;
export type Exact<T extends { [key: string]: unknown }> = { [K in keyof T]: T[K] };
export type MakeOptional<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]?: Maybe<T[SubKey]> };
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]: Maybe<T[SubKey]> };
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string;
  String: string;
  Boolean: boolean;
  Int: number;
  Float: number;
};

export type Agent = {
  __typename?: 'Agent';
  did?: Maybe<Scalars['String']>;
  name?: Maybe<Scalars['String']>;
  email?: Maybe<Scalars['String']>;
};

export type AgentService = {
  __typename?: 'AgentService';
  agent?: Maybe<Agent>;
  isInitialized?: Maybe<Scalars['Boolean']>;
  isUnlocked?: Maybe<Scalars['Boolean']>;
  did?: Maybe<Scalars['String']>;
  didDocument?: Maybe<Scalars['String']>;
  error?: Maybe<Scalars['String']>;
};

export type InitializeAgent = {
  did?: Maybe<Scalars['String']>;
  didDocument?: Maybe<Scalars['String']>;
  keystore?: Maybe<Scalars['String']>;
  passphrase?: Maybe<Scalars['String']>;
};

export type Icon = {
  __typename?: 'Icon';
  code?: Maybe<Scalars['String']>;
};

export type Expression = {
  __typename?: 'Expression';
  url?: Maybe<Scalars['String']>;
  author?: Maybe<Agent>;
  timestamp?: Maybe<Scalars['String']>;
  data?: Maybe<Scalars['String']>;
  icon?: Maybe<Icon>;
  language?: Maybe<Language>;
  proof?: Maybe<ExpressionProof>;
};

export type ExpressionProof = {
  __typename?: 'ExpressionProof';
  signature?: Maybe<Scalars['String']>;
  key?: Maybe<Scalars['String']>;
  valid?: Maybe<Scalars['Boolean']>;
  invalid?: Maybe<Scalars['Boolean']>;
};

export type Link = {
  __typename?: 'Link';
  source?: Maybe<Scalars['String']>;
  predicate?: Maybe<Scalars['String']>;
  target?: Maybe<Scalars['String']>;
};

export type LinkExpression = {
  __typename?: 'LinkExpression';
  author?: Maybe<Agent>;
  timestamp?: Maybe<Scalars['String']>;
  data?: Maybe<Link>;
};

export type LinkQuery = {
  source?: Maybe<Scalars['String']>;
  predicate?: Maybe<Scalars['String']>;
  target?: Maybe<Scalars['String']>;
};

export type Language = {
  __typename?: 'Language';
  name?: Maybe<Scalars['String']>;
  address?: Maybe<Scalars['String']>;
  constructorIcon?: Maybe<Icon>;
  settings?: Maybe<Scalars['String']>;
  settingsIcon?: Maybe<Icon>;
};

export type Perspective = {
  __typename?: 'Perspective';
  uuid?: Maybe<Scalars['String']>;
  name?: Maybe<Scalars['String']>;
  sharedPerspective?: Maybe<SharedPerspective>;
  sharedURL?: Maybe<Scalars['String']>;
  links?: Maybe<Array<Maybe<LinkExpression>>>;
};


export type PerspectiveLinksArgs = {
  query?: Maybe<LinkQuery>;
};

export type SharedPerspective = {
  __typename?: 'SharedPerspective';
  name?: Maybe<Scalars['String']>;
  description?: Maybe<Scalars['String']>;
  type?: Maybe<Scalars['String']>;
  linkLanguages?: Maybe<Array<Maybe<Scalars['String']>>>;
  allowedExpressionLanguages?: Maybe<Array<Maybe<Scalars['String']>>>;
  requiredExpressionLanguages?: Maybe<Array<Maybe<Scalars['String']>>>;
  sharedExpressionLanguages?: Maybe<Array<Maybe<Scalars['String']>>>;
};

export type AddLinkInput = {
  perspectiveUUID?: Maybe<Scalars['String']>;
  link?: Maybe<Scalars['String']>;
};

export type UpdateLinkInput = {
  perspectiveUUID?: Maybe<Scalars['String']>;
  oldLink?: Maybe<Scalars['String']>;
  newLink?: Maybe<Scalars['String']>;
};

export type RemoveLinkInput = {
  perspectiveUUID?: Maybe<Scalars['String']>;
  link?: Maybe<Scalars['String']>;
};

export type CreateExpressionInput = {
  languageAddress?: Maybe<Scalars['String']>;
  content?: Maybe<Scalars['String']>;
};

export type SetLanguageSettingsInput = {
  languageAddress?: Maybe<Scalars['String']>;
  settings?: Maybe<Scalars['String']>;
};

export type AddPerspectiveInput = {
  name?: Maybe<Scalars['String']>;
};

export type UpdatePerspectiveInput = {
  uuid?: Maybe<Scalars['String']>;
  name?: Maybe<Scalars['String']>;
  linksSharingLanguage?: Maybe<Scalars['String']>;
};

export type PublishPerspectiveInput = {
  uuid?: Maybe<Scalars['String']>;
  name?: Maybe<Scalars['String']>;
  description?: Maybe<Scalars['String']>;
  type?: Maybe<Scalars['String']>;
  encrypt?: Maybe<Scalars['Boolean']>;
  passphrase?: Maybe<Scalars['String']>;
  requiredExpressionLanguages?: Maybe<Array<Maybe<Scalars['String']>>>;
  allowedExpressionLanguages?: Maybe<Array<Maybe<Scalars['String']>>>;
  sharedExpressionLanguages?: Maybe<Array<Maybe<Scalars['String']>>>;
};

export type CreateHcExpressionLanguageInput = {
  languagePath?: Maybe<Scalars['String']>;
  dnaNick?: Maybe<Scalars['String']>;
  encrypt?: Maybe<Scalars['Boolean']>;
  passphrase?: Maybe<Scalars['String']>;
};

export type UpdateAgentProfileInput = {
  name?: Maybe<Scalars['String']>;
  email?: Maybe<Scalars['String']>;
};

export type Query = {
  __typename?: 'Query';
  hello?: Maybe<Scalars['String']>;
  agent?: Maybe<AgentService>;
  links?: Maybe<Array<Maybe<LinkExpression>>>;
  expression?: Maybe<Expression>;
  language?: Maybe<Language>;
  languages?: Maybe<Array<Maybe<Language>>>;
  perspectives?: Maybe<Array<Maybe<Perspective>>>;
  perspective?: Maybe<Perspective>;
};


export type QueryLinksArgs = {
  perspectiveUUID?: Maybe<Scalars['String']>;
  query?: Maybe<LinkQuery>;
};


export type QueryExpressionArgs = {
  url?: Maybe<Scalars['String']>;
};


export type QueryLanguageArgs = {
  address?: Maybe<Scalars['String']>;
};


export type QueryLanguagesArgs = {
  filter?: Maybe<Scalars['String']>;
};


export type QueryPerspectiveArgs = {
  uuid?: Maybe<Scalars['String']>;
};

export type Mutation = {
  __typename?: 'Mutation';
  initializeAgent?: Maybe<AgentService>;
  lockAgent?: Maybe<AgentService>;
  unlockAgent?: Maybe<AgentService>;
  updateAgentProfile?: Maybe<AgentService>;
  addPerspective?: Maybe<Perspective>;
  updatePerspective?: Maybe<Perspective>;
  removePerspective?: Maybe<Scalars['Boolean']>;
  publishPerspective?: Maybe<SharedPerspective>;
  addLink?: Maybe<LinkExpression>;
  updateLink?: Maybe<LinkExpression>;
  removeLink?: Maybe<Scalars['Boolean']>;
  createExpression?: Maybe<Scalars['String']>;
  setLanguageSettings?: Maybe<Scalars['Boolean']>;
  openLinkExtern?: Maybe<Scalars['Boolean']>;
  quit?: Maybe<Scalars['Boolean']>;
  createUniqueHolochainExpressionLanguageFromTemplate?: Maybe<Scalars['String']>;
};


export type MutationInitializeAgentArgs = {
  input?: Maybe<InitializeAgent>;
};


export type MutationLockAgentArgs = {
  passphrase?: Maybe<Scalars['String']>;
};


export type MutationUnlockAgentArgs = {
  passphrase?: Maybe<Scalars['String']>;
};


export type MutationUpdateAgentProfileArgs = {
  input?: Maybe<UpdateAgentProfileInput>;
};


export type MutationAddPerspectiveArgs = {
  input?: Maybe<AddPerspectiveInput>;
};


export type MutationUpdatePerspectiveArgs = {
  input?: Maybe<UpdatePerspectiveInput>;
};


export type MutationRemovePerspectiveArgs = {
  uuid?: Maybe<Scalars['String']>;
};


export type MutationPublishPerspectiveArgs = {
  input?: Maybe<PublishPerspectiveInput>;
};


export type MutationAddLinkArgs = {
  input?: Maybe<AddLinkInput>;
};


export type MutationUpdateLinkArgs = {
  input?: Maybe<UpdateLinkInput>;
};


export type MutationRemoveLinkArgs = {
  input?: Maybe<RemoveLinkInput>;
};


export type MutationCreateExpressionArgs = {
  input?: Maybe<CreateExpressionInput>;
};


export type MutationSetLanguageSettingsArgs = {
  input?: Maybe<SetLanguageSettingsInput>;
};


export type MutationOpenLinkExternArgs = {
  url?: Maybe<Scalars['String']>;
};


export type MutationCreateUniqueHolochainExpressionLanguageFromTemplateArgs = {
  input?: Maybe<CreateHcExpressionLanguageInput>;
};

export type Subscription = {
  __typename?: 'Subscription';
  agentUpdated?: Maybe<Agent>;
  perspectiveAdded?: Maybe<Perspective>;
  perspectiveUpdated?: Maybe<Perspective>;
  perspectiveRemoved?: Maybe<Scalars['String']>;
  linkAdded?: Maybe<LinkExpression>;
  linkRemoved?: Maybe<LinkExpression>;
};


export type SubscriptionLinkAddedArgs = {
  perspectiveUUID?: Maybe<Scalars['String']>;
};


export type SubscriptionLinkRemovedArgs = {
  perspectiveUUID?: Maybe<Scalars['String']>;
};