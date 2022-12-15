import { useDisclosure } from '@mantine/hooks';
import { showNotification } from '@mantine/notifications';
import { LanguageMeta } from '@perspect3vism/ad4m';
import { useContext, useEffect, useState } from 'react';
import { Ad4minContext } from '../context/Ad4minContext';
import { isSystemLanguage } from '../util';
import ActionButton from './ActionButton';
import { badge, cardStyle, gridButton, listStyle, MainContainer, MainHeader } from './styles';

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
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  return (
    <div style={MainContainer}>
      <div style={gridButton}>
        <ActionButton 
          tooltip="Publish language"
          onClick={() => setPublishLanguageModalOpen(true)}
          icon="globe"
        />
        <ActionButton 
          tooltip="Install language"
          onClick={() => setInstallLanguageModalOpen(true)}
          icon="download"
        />
      </div>
      <div 
        style={listStyle}
      >
        {languages.map((e, i) => {
          const {language, perspective} = e;
          const isSystem = isSystemLanguage(language!.name)

          return (
          <div key={`language-${language?.address}`} style={{...cardStyle, width: '87%'}}>
            <j-flex a="flex-start" direction="column">
              <div style={{...badge, backgroundColor: isSystem ? '#6e52c2bb' : 'rgba(230, 73, 128, 0.671)'}}>
                {isSystem ? 'System' : 'Installed'}
              </div>
              <j-box p="200"></j-box>
              <j-flex direction='column' style={{marginTop: 4}}>
                <j-text weight="bold" >{language?.name}</j-text>
                <j-flex a="center" j="between">
                  <j-text nomargin variant="body" size="400">{language?.address.length > 28 ? `${language?.address.substring(0, 28)}...` : language?.address}</j-text>
                  <j-box p="100"></j-box>
                  <j-button size="xs" variant="subtle"  onClick={() => console.log('wow')}>
                    <j-icon size="xs" slot="end" name="clipboard"></j-icon>
                  </j-button>
                </j-flex>
              </j-flex>
            </j-flex>
          </div>
        )})}
      </div>
      <j-modal
          size="fullscreen"
          open={publishLanguageModalOpen}
          onToggle={(e: any) => setPublishLanguageModalOpen(e.target.open)}
        >
          <j-box p="400">
            <j-flex gap="200" direction="column">
              <j-text nomargin variant="heading-sm">
                Publish Langauge
              </j-text>
              <j-box p="200"></j-box>
              <j-input
                label="Name"
                size="lg"
                placeholder="ex. Social-Context"
                value={languageName}
                onInput={(e: any) => setLanguageName(e.target.value)}
              >
              </j-input>
              <j-input
                label="Description"
                size="lg"
                placeholder="Describe what the language does here."
                value={languageDescription}
                onInput={(e: any) => setLanguageDescription(e.target.value)}
              >
              </j-input>
              <j-input
                label="Description"
                size="lg"
                placeholder="Describe what the language does here."
                value={languageDescription}
                onInput={(e: any) => setLanguageDescription(e.target.value)}
              >
              </j-input>
              <j-input
                label="Language Bundle Path"
                size="lg"
                placeholder="ex. dev/example/language.js"
                value={languageBundlePath}
                onInput={(e: any) => setLanguageBundlePath(e.target.value)}
              >
              </j-input>
              <j-box p="200"></j-box>
              <j-flex>
                <j-button onClick={() => setInstallLanguageModalOpen(false)}>
                  Cancel
                </j-button>
                <j-box p="200"></j-box>
                <j-button onClick={installLanguage} loading={loading}>
                  Install
                </j-button>
              </j-flex>
            </j-flex>
          </j-box>
      </j-modal>
      <j-modal
          size="fullscreen"
          open={installLanguageModalOpen}
          onToggle={(e: any) => setInstallLanguageModalOpen(e.target.open)}
        >
          <j-box p="400">
            <j-flex gap="200" direction="column">
              <j-text nomargin variant="heading-sm">
                Install Langauge
              </j-text>
              <j-box p="200"></j-box>
              <j-input
                label="Language hash"
                size="lg"
                placeholder="ex. QmUTkvPcyaUGntqfzi3iR1xomADm5yYC2j8hcPdhMHpTem"
                value={languageHash}
                onInput={(e: any) => setLanguageHash(e.target.value)}
              >
              </j-input>
              <j-box p="200"></j-box>
              <j-flex>
                <j-button onClick={() => setInstallLanguageModalOpen(false)}>
                  Cancel
                </j-button>
                <j-box p="200"></j-box>
                <j-button onClick={installLanguage} loading={loading}>
                  Install
                </j-button>
              </j-flex>
            </j-flex>
          </j-box>
      </j-modal>
      <j-modal
          size="fullscreen"
          open={publishLanguageResultModalOpen}
          onToggle={(e: any) => setPublishLanguageResultModalOpen(e.target.open)}
        >
          <j-box p="400">
            <j-flex gap="200" direction="column">
              <j-text nomargin variant="heading-sm">
                Install Langauge
              </j-text>
              <j-box p="200"></j-box>
              <j-text>Name: {publishLanguageResult?.name}</j-text>
              <j-text>Address: {publishLanguageResult?.address}</j-text>
              <j-text>Description: {publishLanguageResult?.description}</j-text>
              <j-text>Author: {publishLanguageResult?.author}</j-text>
              <j-text>Source code link: {publishLanguageResult?.sourceCodeLink}</j-text>
            </j-flex>
            <j-button onClick={() => setPublishLanguageResultModalOpen(false)}>Done</j-button>
          </j-box>
      </j-modal>
    </div>
  )
}

export default Language