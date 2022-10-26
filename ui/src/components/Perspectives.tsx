import { Avatar, Button, Card, Container, Group, List, Modal, MultiSelect, Menu, Select, Space, Switch, TextInput, Title, Text, MediaQuery, Burger } from '@mantine/core';
import { showNotification } from '@mantine/notifications';
import { LanguageHandle, Link, Perspective, PerspectiveProxy } from '@perspect3vism/ad4m';
import { useContext, useEffect, useMemo, useState } from 'react';
import { generateLanguageInitials, sanitizeLink } from '../util';
import { MainContainer, MainHeader } from './styles';
import { Trash } from 'tabler-icons-react';
import { useDisclosure } from '@mantine/hooks';
import { Ad4minContext } from '../context/Ad4minContext';
import { nanoid } from 'nanoid';

type Props = {
  opened: boolean,
  setOpened: (val: boolean) => void
}

const PerspectiveMenu = ({uuid, reload}: {uuid: string, reload: () => {}}) => {
  const {state: {
    client
  }} = useContext(Ad4minContext);
  
  const [opened, handlers] = useDisclosure(false);

  const deletePerspective = async (id: string) => {
    await client!.perspective.remove(id);

    await reload();

    handlers.close();
  }


  return (
    <Menu opened={opened} onOpen={handlers.open} onClose={handlers.close}>
      <Menu.Item 
        icon={<Trash size={16}/>}
        onClick={() => deletePerspective(uuid)}
      >
        Delete
      </Menu.Item>
    </Menu>
  )
}

const Perspectives = (props: Props) => {
  const {state: {
    client
  }} = useContext(Ad4minContext);

  const [perspectiveModalOpen, setPerspectiveModalOpen] = useState(false);
  const [languages, setLanguages] = useState<LanguageHandle[] | null[]>([]);
  const [perspectives, setPerspectives] = useState<PerspectiveProxy[] | null[]>([]);
  const [perspectiveName, setPerspectiveName] = useState("");
  const [isNeighbourhood, setIsNeighbourhood] = useState(false)
  const [linkLanguage, setLinkLanguage] = useState('');
  const [linkLanguages, setLinkLanguages] = useState<string[]>([]);
  const [loading, setLoading] = useState(false);
  
  const fetchPerspective = async () => {
    const perspectives = await client!.perspective.all();

    console.log(perspectives)

    setPerspectives(perspectives)
  }

  const getLanguages = async () => {
    const langs = await client!.languages.all();

    setLanguages(langs);
  }
  
  useEffect(() => {
    fetchPerspective()
    getLanguages()
  }, []);

  const create = async () => {
    setLoading(true)

    const perspective = await client!.perspective.add(perspectiveName);
    
    try {
      if (isNeighbourhood) {
        const templateLangs = [];

        const uid = nanoid()

        const language = (languages as LanguageHandle[]).find((e: LanguageHandle) => e.address === linkLanguage);
  
        const templatedLinkLang = await client!.languages.applyTemplateAndPublish(
          language!.address,
          JSON.stringify({
            uid,
            name: `${perspectiveName}-${language?.name}`
          })
        );
  
        const metaPerspective = await client!.perspective.add(`${perspectiveName}-meta`);

        for (const linkLanguage of linkLanguages) {
          const lang = (languages as LanguageHandle[]).find((e: LanguageHandle) => e.address === linkLanguage);
  
          if (lang) {
            const templatedLang = await client!.languages.applyTemplateAndPublish(
              lang.address,
              JSON.stringify({
                uid,
                name: `${perspectiveName}-${lang.name}`
              })
            );
  
            const link = await client!.perspective.addLink(metaPerspective.uuid, new Link({
              source: 'self',
              target: templatedLang.address,
              predicate: 'language'
            }));
  
            templateLangs.push(sanitizeLink(link))
          }
        }

        await client!.perspective.remove(metaPerspective.uuid)
  
        await client!.neighbourhood.publishFromPerspective(
          perspective.uuid, 
          templatedLinkLang.address,
          new Perspective(templateLangs)
        );
  
        showNotification({
          message: 'Neighbourhood sucessfully created',
        });
  
      } else {
        showNotification({
          message: 'Perspecctive sucessfully created',
        })
      }

      await fetchPerspective()
    } catch (e) {
      client!.perspective.remove(perspective.uuid)
      showNotification({
        message: `Error: ${e}`,
        color: 'red'
      })
    }
    
    setLoading(false)
    setPerspectiveModalOpen(false)
  }

  const langs = useMemo(() => languages.map(e => ({label: e!.name, value: e!.address})), [languages])

  return (
    <Container
      style={MainContainer}
    >
      <div style={MainHeader}>
        <div style={{display: 'flex'}}>          
          <MediaQuery largerThan="sm" styles={{ display: 'none' }}>
            <Burger
              opened={props.opened}
              onClick={() => props.setOpened(!props.opened)}
              size="sm"
              color={'#fff'}
              mr="xl"
            />
          </MediaQuery>
          <Title order={3}>Perspectives</Title>
        </div>
        <Button onClick={() => setPerspectiveModalOpen(true)}>Add Perspective</Button>
      </div>

      <List 
        spacing="xs"
        size="sm"
        center
        pl={20}
        pr={20}
        style={{
          overflowY: 'auto',
          overflowX: 'hidden',
          height: 'auto',
          maxWidth: 900
        }}
      >
        {perspectives.map((e, i) => {
          console.log(e?.name, Boolean(!e?.neighbourhood || e?.name !== 'Agent Profile'))
          return (
          <Card key={`perspectice-${e?.name}`} shadow="sm" withBorder={true} style={{ marginBottom: 20 }}>
            <Group position="apart" align="flex-start">
              <Group align="flex-start">
                <Avatar radius="xl">{generateLanguageInitials(e!.name)}</Avatar>
                <Group direction='column' style={{marginTop: 4}}>
                  <Group  direction='row'>
                    <Text weight="bold">Identifier: </Text>
                    <Text>{e?.uuid}</Text>
                  </Group>
                  <Group  direction='row'>
                    <Text weight="bold">Name: </Text>
                    <Text>{e?.name}</Text>
                  </Group>
                  {e?.sharedUrl && (
                    <>
                      <Group  direction='row'>
                        <Text weight="bold">Shared URL: </Text>
                        <Text>{e?.sharedUrl}</Text>
                      </Group>
                      <Group  direction='row'>
                        <Text weight="bold">Link Language: </Text>
                        <Text>{e?.neighbourhood!.linkLanguage}</Text>
                      </Group>
                    </>
                  )}
                  {e?.neighbourhood ? (
                    <div style={{padding: '2px 20px', background: 'rgb(243, 240, 255)', borderRadius: 30, color: '#845EF7'}}>
                      Neighbourhood
                    </div>
                  ) : (
                    <div style={{padding: '2px 20px', background: '#FFF0F6', borderRadius: 30, color: 'rgb(230, 73, 128)'}}>
                      Perspective
                    </div>
                  )}
                </Group>
              </Group>
              <PerspectiveMenu uuid={e!.uuid} reload={fetchPerspective} />
            </Group>
          </Card>
        )})}
      </List>
      <Modal
        opened={perspectiveModalOpen}
        onClose={() => setPerspectiveModalOpen(false)}
        title="Install Langauge"
        size={700}
        style={{zIndex: 100}}
      >
        <TextInput 
          label="Name"
          required
          placeholder='ex. Test Perspective' 
          radius="md" 
          size="md" 
          onChange={(e) => setPerspectiveName(e.target.value)}
        />
        <Space h="md"  />
        <Switch 
          checked={isNeighbourhood} 
          onChange={(event) => setIsNeighbourhood(event.currentTarget.checked)}
          label="Public Perspective"
        />
        {isNeighbourhood && (
          <>
            <Select
              label="Select link-language"
              placeholder="Pick one"
              data={langs}
              value={linkLanguage}
              onChange={(e) => setLinkLanguage(e as string)}
            />
            <Space h="md"  />
            <MultiSelect
              label="Select Dependent Languages"
              data={langs}
              required
              placeholder="Pick one"
              searchable
              value={linkLanguages}
              onChange={(e) => setLinkLanguages(e as string[])}
            />
          </>
        )}
        <Space h="md"  />
        <Group direction='row'>
          <Button onClick={() => setPerspectiveModalOpen(false)}>
            Cancel
          </Button>
          <Button onClick={create} loading={loading}>
            Create
          </Button>
        </Group>
      </Modal>
    </Container>
  )
}

export default Perspectives