import { useState, useEffect, useRef } from "react";
import { Ad4mClient } from "@coasys/ad4m";

type UUID = string;

interface PerspectiveProxy {
    uuid: UUID;
    sharedUrl: string;
    addListener(event: string, callback: Function): void;
    removeListener(event: string, callback: Function): void;
}

export function usePerspectives(client: Ad4mClient) {
    const [perspectives, setPerspectives] = useState<{ [x: UUID]: PerspectiveProxy }>({});
    const [neighbourhoods, setNeighbourhoods] = useState<{ [x: UUID]: PerspectiveProxy }>({});
    const onAddedLinkCbs = useRef<Function[]>([]);
    const onRemovedLinkCbs = useRef<Function[]>([]);
    const hasFetched = useRef(false);

    useEffect(() => {
        const fetchPerspectives = async () => {
            if (hasFetched.current) return;
            hasFetched.current = true;

            const allPerspectives = await client.perspective.all();
            const newPerspectives: { [x: UUID]: PerspectiveProxy } = {};

            allPerspectives.forEach((p) => {
                newPerspectives[p.uuid] = p;
                addListeners(p);
            });

            setPerspectives(newPerspectives);
        };

        const addListeners = (p: PerspectiveProxy) => {
            p.addListener("link-added", (link: any) => {
                onAddedLinkCbs.current.forEach((cb) => {
                    cb(p, link);
                });
            });

            p.addListener("link-removed", (link: any) => {
                onRemovedLinkCbs.current.forEach((cb) => {
                    cb(p, link);
                });
            });
        };

        const perspectiveUpdatedListener = async (handle: any) => {
            const perspective = await client.perspective.byUUID(handle.uuid);
            if (perspective) {
                setPerspectives((prevPerspectives) => ({
                    ...prevPerspectives,
                    [handle.uuid]: perspective,
                }));
            }
        };

        const perspectiveAddedListener = async (handle: any) => {
            const perspective = await client.perspective.byUUID(handle.uuid);
            if (perspective) {
                setPerspectives((prevPerspectives) => ({
                    ...prevPerspectives,
                    [handle.uuid]: perspective,
                }));
                addListeners(perspective);
            }
        };

        const perspectiveRemovedListener = (uuid: UUID) => {
            setPerspectives((prevPerspectives) => {
                const newPerspectives = { ...prevPerspectives };
                delete newPerspectives[uuid];
                return newPerspectives;
            });
        };

        fetchPerspectives();

        // @ts-ignore
        client.perspective.addPerspectiveUpdatedListener(perspectiveUpdatedListener);
         // @ts-ignore
        client.perspective.addPerspectiveAddedListener(perspectiveAddedListener);
         // @ts-ignore
        client.perspective.addPerspectiveRemovedListener(perspectiveRemovedListener);

        return () => {
             // @ts-ignore
            client.perspective.removePerspectiveUpdatedListener(perspectiveUpdatedListener);
             // @ts-ignore
            client.perspective.removePerspectiveAddedListener(perspectiveAddedListener);
             // @ts-ignore
            client.perspective.removePerspectiveRemovedListener(perspectiveRemovedListener);
        };
    }, []);

    useEffect(() => {
        const newNeighbourhoods = Object.keys(perspectives).reduce((acc, key) => {
            if (perspectives[key]?.sharedUrl) {
                return {
                    ...acc,
                    [key]: perspectives[key],
                };
            } else {
                return acc;
            }
        }, {});

        setNeighbourhoods(newNeighbourhoods);
    }, [perspectives]);

    function onLinkAdded(cb: Function) {
        onAddedLinkCbs.current.push(cb);
    }

    function onLinkRemoved(cb: Function) {
        onRemovedLinkCbs.current.push(cb);
    }

    return { perspectives, neighbourhoods, onLinkAdded, onLinkRemoved };
}
