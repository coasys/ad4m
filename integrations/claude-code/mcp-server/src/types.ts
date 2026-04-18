export interface AgentStatus {
  isInitialized: boolean;
  isUnlocked: boolean;
  did: string | null;
}

export interface Perspective {
  uuid: string;
  name: string;
  sharedUrl: string | null;
  state: string | null;
}

export interface LinkData {
  source: string;
  predicate: string;
  target: string;
}

export interface LinkExpression {
  author: string;
  timestamp: string;
  data: LinkData;
}

export interface Neighbourhood {
  author: string;
  timestamp: string;
}

export interface PerspectiveWithNeighbourhood extends Perspective {
  neighbourhood: Neighbourhood | null;
}
