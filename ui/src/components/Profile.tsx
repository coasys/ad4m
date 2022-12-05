import { Avatar, Burger, Button, Card, Container, Group, List, MediaQuery, Modal, Space, ThemeIcon, Title } from '@mantine/core';
import { Agent, Literal } from '@perspect3vism/ad4m';
import { useContext, useEffect, useState } from 'react';
import { CircleCheck } from 'tabler-icons-react';
import { PREDICATE_FIRSTNAME, PREDICATE_LASTNAME, PREDICATE_USERNAME } from '../constants/triples';
import { MainContainer, MainHeader } from './styles';
import { Ad4minContext } from '../context/Ad4minContext';
import { buildAd4mClient } from '../util';
import { useCallback } from 'react';
import CardItems from './CardItems';

type Props = {
  did: String,
  opened: boolean,
  setOpened: (val: boolean) => void
}

const Profile = (props: Props) => {
  const {state: {
    url
  }} = useContext(Ad4minContext);

  const [trustedAgents, setTrustedAgents] = useState<any[]>([]);

  const [trustedAgentModalOpen, settrustedAgentModalOpen] = useState(false);
  
  const [profile, setProfile] = useState({
    firstName: "",
    lastName: "",
    username: ""
  });

  const getTrustedAgents = useCallback(async () => {
    if (url) {
      const client = await buildAd4mClient(url);
      const trustedAgents = await client!.runtime.getTrustedAgents()
      
      const tempTempAgents = [];
      
      for (const agent of trustedAgents) {
        const fetchedAgent = await client!.agent.byDID(agent)
  
        if (fetchedAgent) {
          const profile = await fetchProfile(fetchedAgent)
    
          tempTempAgents.push({did: agent, ...profile});
        } else {
          tempTempAgents.push({did: agent});
        }
  
      }
  
      setTrustedAgents(tempTempAgents);
    }
  }, [url])

  const fetchProfile = async (agent: Agent) => {
    const tempProfile = {
      firstName: "",
      lastName: "",
      username: ""
    }

    for (const { data: {source, predicate, target} } of agent.perspective?.links!) {
      if (source === agent.did) {
        if (predicate === PREDICATE_FIRSTNAME) {
          tempProfile.firstName = Literal.fromUrl(target).get()
        } else if (predicate === PREDICATE_LASTNAME) {
          tempProfile.lastName = Literal.fromUrl(target).get();
        } else if (predicate === PREDICATE_USERNAME) {
          tempProfile.username = Literal.fromUrl(target).get();
        }
      }
    }

    return tempProfile;
  }

  const fetchCurrentAgentProfile = useCallback(async () => {
    if (url) {
      const client = await buildAd4mClient(url);
      const agent = await client!.agent.me();
  
      const profile = await fetchProfile(agent);
      
      setProfile(profile);
    }
  }, [url])

  useEffect(() => {
    fetchCurrentAgentProfile();
    getTrustedAgents();
  }, [fetchCurrentAgentProfile, getTrustedAgents])

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
          <Title order={3}>Agent Profile</Title>
        </div>
        <Button onClick={() => settrustedAgentModalOpen(true)}>Trusted Agents</Button>
      </div>
      <Container
        style={{ marginLeft: 10, marginTop: 12 }}
      >
        <Space h="md" />
                
        <CardItems 
          title={'Agent DID'}
          value={props.did as string}
          titleUnderline
        />
        <Space h="md" />
        <CardItems 
          title={'Username'}
          value={profile?.username}
          titleUnderline
        />
        <Space h="md" />
        <CardItems 
          title={'Name'}
          value={`${profile.firstName} ${profile.lastName}`}
          titleUnderline
        />
        <Space h="md" />
      </Container>
      <Modal
        opened={trustedAgentModalOpen}
        onClose={() => settrustedAgentModalOpen(false)}
        title="Trusted Agents"
        size={700}
      >
        <List
          spacing="xs"
          size="sm"
          center
          icon={
            <ThemeIcon color="teal" size={24} radius="xl">
              <CircleCheck size={16} />
            </ThemeIcon>
          }
        >
          {trustedAgents.map((e, i) => (
            <Card key={`trusted-agent-${e.did}`} shadow="sm" withBorder={true} style={{ marginBottom: trustedAgents.length-1 === i ? 0 : 20 }}>
              <Group align="flex-start">
                <Avatar radius="xl"></Avatar>
                <Group direction='column' style={{marginTop: 4}}>
                  <CardItems 
                    title={'DID'}
                    value={e.did}
                  />
                  {e.username && (<CardItems 
                    title={'Username'}
                    value={e.username}
                  />)}
                  {(e.firstName || e.lastName) && (<CardItems 
                    title={'Name'}
                    value={`${e.firstName || ""} ${e.lastName || ""}`}
                  />)}
                </Group>
              </Group>
            </Card>
          ))}
        </List>
      </Modal>
    </Container>
  )
}

export default Profile