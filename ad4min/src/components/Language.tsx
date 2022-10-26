import { Button, Container, TextInput, Text, Modal, MultiSelect, Space, Group, List, Card, Avatar, Title, Menu, MediaQuery, Burger } from '@mantine/core';
import { useDisclosure } from '@mantine/hooks';
import { showNotification } from '@mantine/notifications';
import { LanguageMeta } from '@perspect3vism/ad4m';
import { useContext, useEffect, useState } from 'react';
import { Download, Upload } from 'tabler-icons-react';
import { Ad4minContext } from '../context/Ad4minContext';
import { generateLanguageInitials, isSystemLanguage } from '../util';
import { MainContainer, MainHeader } from './styles';

type Props = {
  opened: boolean,
  setOpened: (val: boolean) => void
}

const Language = (props: Props) => {
  const {state: {
    client
  }} = useContext(Ad4minContext);

  const [languages, setLanguages] = useState<any[] | null[]>([]);
  const [loading, setLoading] = useState(false);
  const [installLanguageModalOpen, setInstallLanguageModalOpen] = useState(false);
  const [publishLanguageModalOpen, setPublishLanguageModalOpen] = useState(false);
  const [publishLanguageResultModalOpen, setPublishLanguageResultModalOpen] = useState(false);
  const [publishLanguageResult, setPublishLanguageResult] = useState<LanguageMeta | null>(null);


  const [languageHash, setLanguageHash] = useState("");
  const [languageName, setLanguageName] = useState("");
  const [languageDescription, setLanguageDescription] = useState("");
  const [languageSourceLink, setLanguageSourceLink] = useState("");
  const [languageBundlePath, setLanguageBundlePath] = useState("");
  const [data, setData] = useState<any[]>([]);

  const [opened, handlers] = useDisclosure(false);

  const publishLanguage = async () => {
    setLoading(true);
    if (languageBundlePath) {
      const installedLanguage = await client!.languages.publish(languageBundlePath, {
        name: languageName,
        description: languageDescription,
        possibleTemplateParams: data,
        sourceCodeLink: languageSourceLink
      });

      console.log(installedLanguage);

      setPublishLanguageModalOpen(false)

      setPublishLanguageResultModalOpen(true)

      setPublishLanguageResult(installedLanguage);

    } else {
      showNotification({
        message: 'Language file missing',
        color: 'red'
      })
    }
    setLoading(false);
  }

  const installLanguage = async () => {
    setLoading(true);
    try {
      if (languageBundlePath) {
        await client!.languages.byAddress(languageHash)
  
        await getLanguages()
  
        
        setInstallLanguageModalOpen(false)
        
        showNotification({
          message: 'Language sucessfully installed',
        })
      } else {
        showNotification({
          message: 'Language file missing',
          color: 'red'
        })
      }

      setLoading(false);
    } catch (e) {
      setLoading(false);
      throw e;
    }
  }

  const getLanguages = async () => {
    const langs = await client!.languages.all();

    const perspectives = await client!.perspective.all();

    const tempLangs = [];
    
    for (const lang of langs) {
      const found = perspectives.find(p => {
        if (p.neighbourhood) {
          if (p.neighbourhood.linkLanguage === lang.address) {
            return true;
          } else {
            return p.neighbourhood.meta.links.filter(l => l.data.predicate === 'language')
              .find(l => l.data.target === lang.address)
          }
        }

        return false;
      });

      tempLangs.push({language: lang, perspective: found})
    }

    setLanguages(tempLangs);
  }

  useEffect(() => {
    getLanguages();
  }, [])

  return (
    <Container style={MainContainer}>
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
          <Title order={3}>Languages</Title>
        </div>
        <Menu opened={opened} onOpen={handlers.open} onClose={handlers.close}>
          <Menu.Item 
            icon={<Upload size={16}/>}
            onClick={() => setPublishLanguageModalOpen(true)}
          >
            Publish Language
          </Menu.Item>
          <Menu.Item 
            icon={<Download size={16}/>}
            onClick={() => setInstallLanguageModalOpen(true)}
          >
            Install Language
          </Menu.Item>
        </Menu>
      </div>
      <List 
        spacing="xs"
        size="sm"
        center
        pl={20}
        pr={20}
        mt={20}
        mr={20}
        style={{
          overflowY: 'auto',
          overflowX: 'hidden',
          height: 'auto',
          maxWidth: 920
        }}
      >
        {languages.map((e, i) => {
          const {language, perspective} = e;
          const isSystem = isSystemLanguage(language!.name)

          return (
          <Card key={`language-${language?.address}`} shadow="sm" withBorder={true} style={{ marginBottom: 20 }}>
            <Group align="flex-start">
              <Avatar radius="xl">{generateLanguageInitials(language!.name)}</Avatar>
              <Group direction='column' style={{marginTop: 4}}>
                <Group  direction='row'>
                  <Text weight="bold">Address: </Text>
                  <Text>{language?.address}</Text>
                </Group>
                <Group  direction='row'>
                  <Text weight="bold">Name: </Text>
                  <Text>{language?.name}</Text>
                </Group>
                {perspective && (
                  <Group  direction='row'>
                    <Text weight="bold">Perspective: </Text>
                    <Text>{perspective?.name}</Text>
                  </Group>
                )}
                {isSystem ? (
                  <div style={{padding: '4px 12px', background: 'rgb(243, 240, 255)', borderRadius: 30, color: '#845EF7'}}>
                    System
                  </div>
                ) : (
                  <div style={{padding: '4px 12px', background: '#FFF0F6', borderRadius: 30, color: 'rgb(230, 73, 128)'}}>
                  Installed
                  </div>
                )}
              </Group>
            </Group>
          </Card>
        )})}
      </List>
      <Modal
        opened={publishLanguageModalOpen}
        onClose={() => setPublishLanguageModalOpen(false)}
        title="Install Langauge"
        size={700}
        style={{zIndex: 100}}
      >
        <TextInput 
          label="Name"
          required
          placeholder='ex. Social-Context' 
          radius="md" 
          size="md" 
          onChange={(e) => setLanguageName(e.target.value)}
        />
        <Space h="md"  />
        <TextInput 
          label="Description" 
          required
          placeholder='Describe what the language does here.' 
          radius="md" 
          size="md" 
          onChange={(e) => setLanguageDescription(e.target.value)}
        />
        <Space h="md"  />
        <TextInput 
          label="Souce Code Link" 
          required
          placeholder='ex. www.example.com' 
          radius="md" 
          size="md" 
          onChange={(e) => setLanguageSourceLink(e.target.value)}
        />
        <Space h="md"  />
        <MultiSelect
          label="Params"
          data={data}
          required
          placeholder="Add Items"
          searchable
          creatable
          getCreateLabel={(query) => `+ Create ${query}`}
          onCreate={(query) => setData((current: any) => [...current, query])}
        />
        <Space h="md"  />
        <TextInput 
          label="Language Bundle Path" 
          required
          placeholder='ex. dev/example/language.js' 
          radius="md" 
          size="md" 
          onChange={(e) => setLanguageBundlePath(e.target.value)}
        />
        <Space h="md"  />
        <Group direction='row'>
          <Button onClick={() => setInstallLanguageModalOpen(false)}>
            Cancel
          </Button>
          <Button onClick={publishLanguage} loading={loading}>
            Install
          </Button>
        </Group>
      </Modal>
      <Modal
        opened={installLanguageModalOpen}
        onClose={() => setInstallLanguageModalOpen(false)}
        title="Install Langauge"
        size={700}
        style={{zIndex: 100}}
      >
        <TextInput 
          label="Language hash"
          required
          placeholder='ex. QmUTkvPcyaUGntqfzi3iR1xomADm5yYC2j8hcPdhMHpTem' 
          radius="md" 
          size="md" 
          onChange={(e) => setLanguageHash(e.target.value)}
        />
        <Space h="md"  />
        <Group direction='row'>
          <Button onClick={() => setInstallLanguageModalOpen(false)}>
            Cancel
          </Button>
          <Button onClick={installLanguage} loading={loading}>
            Install
          </Button>
        </Group>
      </Modal>
      <Modal
        opened={publishLanguageResultModalOpen}
        onClose={() => setPublishLanguageResultModalOpen(false)}
        title="Install Langauge"
        size={700}
        style={{zIndex: 100}}
      >
        <Text>Name: {publishLanguageResult?.name}</Text>
        <Text>Address: {publishLanguageResult?.address}</Text>
        <Text>Description: {publishLanguageResult?.description}</Text>
        <Text>Author: {publishLanguageResult?.author}</Text>
        <Text>Source code link: {publishLanguageResult?.sourceCodeLink}</Text>
        <Space h="md"  />
        <Button onClick={() => setPublishLanguageResultModalOpen(false)}>
          Done
        </Button>
      </Modal>
    </Container>
  )
}

export default Language