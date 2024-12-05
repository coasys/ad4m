import { useContext, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Ad4minContext } from "../context/Ad4minContext";
import { buildAd4mClient } from "../util";

export function Connect() {
  const {
    state: { connectedLoading, connected, isUnlocked },
    methods: { configureEndpoint },
  } = useContext(Ad4minContext);

  const [url, setURL] = useState("");
  const [urlError, setURLError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  let navigate = useNavigate();

  const onUrlChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    const { value } = event.target;
    setURL(value);
  };

  const onInitialize = async () => {
    setLoading(true);

    return new Promise(async (resolve, reject) => {
      if (!url) {
        setURLError("URL is required");
      } else if (
        !/^(wss?:\/\/)([0-9]{1,3}(?:\.[0-9]{1,3}){3}|[a-zA-Z]+):([0-9]{1,5})(?:\/[a-zA-Z]{0,100})$/.test(
          url
        )
      ) {
        setURLError("Invalid websocket URL");
      } else {
        try {
          const client = await buildAd4mClient(url!, false);

          const id = setTimeout(() => {
            resolve(true);

            // TODO: Add notification that we failed to connect to the endpoint

            setLoading(false);
          }, 2000);

          let ad4mInfo = await client.runtime.info();
          console.log("AD4M JS Client built, got info: ", ad4mInfo);

          clearTimeout(id);

          configureEndpoint(url);

          resolve(true);
        } catch (e) {
          // TODO: Add notification that we failed to connect to the endpoint

          reject();
        } finally {
          setLoading(false);
        }
      }
      setLoading(false);
      resolve(true);
    });
  };

  useEffect(() => {
    if (connected && !isUnlocked) {
      navigate("/login");
    }
  }, [connected, isUnlocked, navigate]);

  return (
    <j-flex direction="column" a="center" gap="500" style={{ margin: "auto" }}>
      {connectedLoading ? (
        <j-spinner size="lg"></j-spinner>
      ) : (
        <>
          <j-input
            label="Ad4m URL"
            placeholder="ws://www.example.com/graphql"
            radius="md"
            size="md"
            onInput={onUrlChange}
            required
            error={urlError}
          />
          <j-button onClick={onInitialize} variant="primary" loading={loading}>
            Initialize Client
          </j-button>
        </>
      )}
    </j-flex>
  );
}
