import { useState, useEffect, useMemo, useRef } from "react";
import { Ad4mClient, PerspectiveHandle, PerspectiveProxy } from "@coasys/ad4m";
import { useLive } from "./useLive";

type UUID = string;

/**
 * The agent's perspectives, read from the client's mirror: the executor's
 * `perspective-added`, `-updated` and `-removed` events change the list and re-render, a
 * reconnect reloads it. Hooks with one client share the list; two clients share nothing.
 * Link callbacks fire for every perspective listed.
 */
export function usePerspectives(client: Ad4mClient) {
    const handles = useLive(useMemo(() => client.perspective.live.all(), [client]))?.value;
    const [perspectives, setPerspectives] = useState<{ [x: UUID]: PerspectiveProxy }>({});
    const onAddedLinkCbs = useRef<Function[]>([]);
    const onRemovedLinkCbs = useRef<Function[]>([]);
    /** The proxy built for each listed handle, with the link listeners it carries. */
    const held = useRef(new Map<UUID, { handle: PerspectiveHandle; proxy: PerspectiveProxy }>());

    useEffect(() => {
        if (!handles) return;
        const next = new Map<UUID, { handle: PerspectiveHandle; proxy: PerspectiveProxy }>();
        for (const handle of handles) {
            const prev = held.current.get(handle.uuid);
            if (prev?.handle === handle) {
                next.set(handle.uuid, prev);
                continue;
            }
            prev?.proxy.dispose();
            const proxy = new PerspectiveProxy(handle, client.perspective);
            addListeners(proxy);
            next.set(handle.uuid, { handle, proxy });
        }
        for (const [uuid, { proxy }] of held.current) if (!next.has(uuid)) proxy.dispose();
        held.current = next;
        setPerspectives(Object.fromEntries([...next].map(([uuid, { proxy }]) => [uuid, proxy])));
    }, [handles]);

    // The listeners go with the component.
    useEffect(() => () => {
        held.current.forEach(({ proxy }) => proxy.dispose());
        held.current = new Map();
    }, []);

    const addListeners = (p: PerspectiveProxy) => {
        p.on("link-added", ({ link }) => {
            onAddedLinkCbs.current.forEach((cb) => {
                cb(p, link);
            });
        });

        p.on("link-removed", ({ link }) => {
            onRemovedLinkCbs.current.forEach((cb) => {
                cb(p, link);
            });
        });

        // An update replaces a link: the old one leaves, the new one arrives.
        p.on("link-updated", ({ oldLink, newLink }) => {
            onRemovedLinkCbs.current.forEach((cb) => cb(p, oldLink));
            onAddedLinkCbs.current.forEach((cb) => cb(p, newLink));
        });
    };

    const neighbourhoods = useMemo(
        () => Object.fromEntries(Object.entries(perspectives).filter(([, p]) => p.sharedUrl)),
        [perspectives]
    );

    function onLinkAdded(cb: Function) {
        onAddedLinkCbs.current.push(cb);
    }

    function onLinkRemoved(cb: Function) {
        onRemovedLinkCbs.current.push(cb);
    }

    return { perspectives, neighbourhoods, onLinkAdded, onLinkRemoved };
}
