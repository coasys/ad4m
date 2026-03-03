import React, { useState, useEffect } from 'react';
import { usePerspectives } from './usePerspectives';
import { Ad4mClient, PerspectiveProxy } from '@coasys/ad4m';

export function usePerspective(client: Ad4mClient, uuid: string | Function) {
  const [uuidState, setUuidState] = useState(typeof uuid === 'function' ? uuid() : uuid);

  const { perspectives } = usePerspectives(client);

  const [data, setData] = useState<{ perspective: PerspectiveProxy | null, synced: boolean }>({
    perspective: null,
    synced: false,
  });

  useEffect(() => {
    const pers = perspectives[uuidState];
    setData(prevData => ({ ...prevData, perspective: pers }));
  }, [perspectives, uuidState]);

  useEffect(() => {
    if (typeof uuid === 'function') {
      setUuidState(uuid());
    } else {
      setUuidState(uuid);
    }
  }, [uuid]);

  return { data };
}