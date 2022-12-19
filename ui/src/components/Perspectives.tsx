import { Group, Menu } from "@mantine/core";
import { showNotification } from "@mantine/notifications";
import {
  LanguageHandle,
  Link,
  Perspective,
  PerspectiveProxy,
} from "@perspect3vism/ad4m";
import { useContext, useEffect, useMemo, useState } from "react";
import { sanitizeLink } from "../util";
import {
  badge,
  cardStyle,
  gridButton,
  listStyle,
  MainContainer,
  MainHeader,
} from "./styles";
import { Trash } from "tabler-icons-react";
import { useDisclosure } from "@mantine/hooks";
import { Ad4minContext } from "../context/Ad4minContext";
import { nanoid } from "nanoid";
import ActionButton from "./ActionButton";

type Props = {
  opened: boolean;
  setOpened: (val: boolean) => void;
};

const PerspectiveMenu = ({
  uuid,
  reload,
}: {
  uuid: string;
  reload: () => {};
}) => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [opened, handlers] = useDisclosure(false);

  const deletePerspective = async (id: string) => {
    await client!.perspective.remove(id);

    await reload();

    handlers.close();
  };

  return (
    <Menu opened={opened} onOpen={handlers.open} onClose={handlers.close}>
      <Menu.Item
        icon={<Trash size={16} />}
        onClick={() => deletePerspective(uuid)}
      >
        Delete
      </Menu.Item>
    </Menu>
  );
};

const Perspectives = (props: Props) => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [perspectiveModalOpen, setPerspectiveModalOpen] = useState(false);
  const [languages, setLanguages] = useState<LanguageHandle[] | null[]>([]);
  const [perspectives, setPerspectives] = useState<PerspectiveProxy[] | null[]>(
    []
  );
  const [perspectiveName, setPerspectiveName] = useState("");
  const [isNeighbourhood, setIsNeighbourhood] = useState(false);
  const [linkLanguage, setLinkLanguage] = useState("");
  const [linkLanguages, setLinkLanguages] = useState<string[]>([]);
  const [loading, setLoading] = useState(false);

  const fetchPerspective = async () => {
    const perspectives = await client!.perspective.all();

    console.log(perspectives);

    setPerspectives(perspectives);
  };

  const getLanguages = async () => {
    const langs = await client!.languages.all();

    setLanguages(langs);
  };

  useEffect(() => {
    fetchPerspective();
    getLanguages();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const create = async () => {
    setLoading(true);

    const perspective = await client!.perspective.add(perspectiveName);

    try {
      if (isNeighbourhood) {
        const templateLangs = [];

        const uid = nanoid();

        const language = (languages as LanguageHandle[]).find(
          (e: LanguageHandle) => e.address === linkLanguage
        );

        const templatedLinkLang =
          await client!.languages.applyTemplateAndPublish(
            language!.address,
            JSON.stringify({
              uid,
              name: `${perspectiveName}-${language?.name}`,
            })
          );

        const metaPerspective = await client!.perspective.add(
          `${perspectiveName}-meta`
        );

        for (const linkLanguage of linkLanguages) {
          const lang = (languages as LanguageHandle[]).find(
            (e: LanguageHandle) => e.address === linkLanguage
          );

          if (lang) {
            const templatedLang =
              await client!.languages.applyTemplateAndPublish(
                lang.address,
                JSON.stringify({
                  uid,
                  name: `${perspectiveName}-${lang.name}`,
                })
              );

            const link = await client!.perspective.addLink(
              metaPerspective.uuid,
              new Link({
                source: "self",
                target: templatedLang.address,
                predicate: "language",
              })
            );

            templateLangs.push(sanitizeLink(link));
          }
        }

        await client!.perspective.remove(metaPerspective.uuid);

        await client!.neighbourhood.publishFromPerspective(
          perspective.uuid,
          templatedLinkLang.address,
          new Perspective(templateLangs)
        );

        showNotification({
          message: "Neighbourhood sucessfully created",
        });
      } else {
        showNotification({
          message: "Perspecctive sucessfully created",
        });
      }

      await fetchPerspective();
    } catch (e) {
      client!.perspective.remove(perspective.uuid);
      showNotification({
        message: `Error: ${e}`,
        color: "red",
      });
    }

    setLoading(false);
    setPerspectiveModalOpen(false);
  };

  const langs = useMemo(
    () => languages.map((e) => ({ label: e!.name, value: e!.address })),
    [languages]
  );

  return (
    <div style={MainContainer}>
      <div style={gridButton}>
        <ActionButton
          title="Add perspective"
          onClick={() => setPerspectiveModalOpen(true)}
          icon="folder-plus"
        />
      </div>
      <div style={listStyle}>
        {perspectives.map((e, i) => {
          return (
            <div
              key={`perspectice-${e?.name}`}
              style={{ ...cardStyle, width: "87%" }}
            >
              <j-flex align="flex-start">
                <j-flex align="flex-start">
                  <j-flex a="flex-start" direction="column">
                    <div
                      style={{
                        ...badge,
                        backgroundColor: e?.neighbourhood
                          ? "#6e52c2bb"
                          : "rgba(230, 73, 128, 0.671)",
                      }}
                    >
                      {e?.neighbourhood ? "Neighbourhood" : "Perspective"}
                    </div>
                    <j-box p="200"></j-box>
                    <j-text weight="bold">{e?.name}</j-text>
                    <j-flex a="center" j="between">
                      <j-text nomargin variant="body" size="400">
                        {e?.uuid?.length || 0 > 32
                          ? `${e?.uuid?.substring(0, 32)}...`
                          : e?.uuid}
                      </j-text>
                      <j-box p="100"></j-box>
                      <j-button
                        size="xs"
                        variant="subtle"
                        onClick={() => console.log("wow")}
                      >
                        <j-icon size="xs" slot="end" name="clipboard"></j-icon>
                      </j-button>
                    </j-flex>
                    <j-box p="200"></j-box>
                    {e?.sharedUrl && (
                      <>
                        <j-flex a="center" j="between">
                          <j-text nomargin variant="body" size="400">
                            {e?.sharedUrl?.length > 28
                              ? `${e?.sharedUrl?.substring(0, 28)}...`
                              : e?.sharedUrl}
                          </j-text>
                          <j-box p="100"></j-box>
                          <j-button
                            size="xs"
                            variant="subtle"
                            onClick={() => console.log("wow")}
                          >
                            <j-icon
                              size="xs"
                              slot="end"
                              name="clipboard"
                            ></j-icon>
                          </j-button>
                        </j-flex>
                      </>
                    )}
                  </j-flex>
                </j-flex>
                <div style={{ position: "absolute", top: 10, right: 10 }}>
                  <PerspectiveMenu uuid={e!.uuid} reload={fetchPerspective} />
                </div>
              </j-flex>
            </div>
          );
        })}
      </div>
      {perspectiveModalOpen && (
        <j-modal
          size="fullscreen"
          open={perspectiveModalOpen}
          onToggle={(e: any) => setPerspectiveModalOpen(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Create Perspective
              </j-text>
            </j-box>
            <j-input
              label="Name"
              size="lg"
              placeholder="ex. Test Perspective"
              value={perspectiveName}
              onInput={(e: any) => setPerspectiveName(e.target.value)}
            ></j-input>
            <j-toggle
              style={{ width: "100%" }}
              full
              size="lg"
              variant="primary"
              checked={isNeighbourhood}
              onChange={(e: any) => setIsNeighbourhood(e.target.checked)}
            >
              Public Perspective
            </j-toggle>
            {isNeighbourhood && (
              <>
                <j-select
                  value={linkLanguage}
                  onChange={(e: any) => setLinkLanguage(e.target.value)}
                >
                  {langs.map((e) => (
                    <j-menu-item label={e.label} value={e.value}>
                      {" "}
                      {e.label}{" "}
                    </j-menu-item>
                  ))}
                </j-select>
                <j-select
                  value={linkLanguages}
                  onChange={(e: any) => setLinkLanguages(e.target.value)}
                >
                  {langs.map((e) => (
                    <j-menu-item label={e.label} value={e.value}>
                      {" "}
                      {e.label}{" "}
                    </j-menu-item>
                  ))}
                </j-select>
              </>
            )}

            <j-box p="200"></j-box>
            <j-flex>
              <j-button onClick={() => setPerspectiveModalOpen(false)}>
                Cancel
              </j-button>
              <j-box p="200"></j-box>
              <j-button onClick={create} loading={loading}>
                Install
              </j-button>
            </j-flex>
          </j-box>
        </j-modal>
      )}
    </div>
  );
};

export default Perspectives;
