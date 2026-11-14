import {
    membership_links, membership_admits, room_key_event
} from 'ext:core/ops';

// A link language's way into its space's membership (membership_extension.rs). Each call
// acts for the perspective of the calling language's own link language.
((globalThis) => {
    globalThis.MEMBERSHIP = {
        // The membership links the language synced: [{ hash, counted, reason? }], in order.
        links: (links) => {
            return membership_links(links);
        },
        // Whether the space admits the key now: { admitted, reason? }.
        admits: (didKey) => {
            return membership_admits(didKey);
        },
        // { kind: "dropped" | "rotated", removedKeys, version? }
        roomKeyEvent: (event) => {
            return room_key_event(event);
        },
    };
  })(globalThis);
