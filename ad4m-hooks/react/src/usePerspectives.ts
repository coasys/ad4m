import { useState, useEffect, useRef } from "react";
import { Ad4mClient, EventMap, PerspectiveProxy } from "@coasys/ad4m";

type UUID = string;

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
        };

        const perspectiveUpdatedListener = async ({ perspective: handle }: EventMap["perspective-updated"]) => {
            const perspective = await client.perspective.byUUID(handle.uuid);
            if (perspective) {
                setPerspectives((prevPerspectives) => ({
                    ...prevPerspectives,
                    [handle.uuid]: perspective,
                }));
            }
        };

        const perspectiveAddedListener = async ({ perspective: handle }: EventMap["perspective-added"]) => {
            const perspective = await client.perspective.byUUID(handle.uuid);
            if (perspective) {
                setPerspectives((prevPerspectives) => ({
                    ...prevPerspectives,
                    [handle.uuid]: perspective,
                }));
                addListeners(perspective);
            }
        };

        const perspectiveRemovedListener = ({ perspectiveUuid: uuid }: EventMap["perspective-removed"]) => {
            setPerspectives((prevPerspectives) => {
                const newPerspectives = { ...prevPerspectives };
                delete newPerspectives[uuid];
                return newPerspectives;
            });
        };

        fetchPerspectives();

        const releases = [
            client.on("perspective-updated", perspectiveUpdatedListener),
            client.on("perspective-added", perspectiveAddedListener),
            client.on("perspective-removed", perspectiveRemovedListener),
        ];

        return () => releases.forEach((release) => release());
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
