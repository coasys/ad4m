import { Agent, Literal } from '@perspect3vism/ad4m';
import { useContext, useEffect, useState } from 'react';
import { PREDICATE_FIRSTNAME, PREDICATE_LASTNAME, PREDICATE_USERNAME } from '../constants/triples';
import { cardStyle, gridButton, MainContainer, MainHeader } from './styles';
import { Ad4minContext } from '../context/Ad4minContext';
import { buildAd4mClient } from '../util';
import { useCallback } from 'react';
import CardItems from './CardItems';

type Props = {
  did: String,
  opened: boolean,
  setOpened: (val: boolean) => void
}

export const fetchProfile = async (agent: Agent) => {
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

  console.log(trustedAgentModalOpen)

  return (
    <div style={MainContainer}>
      <div style={gridButton}>
        <j-tooltip title="Trusted agents" placement="bottom">
          <j-button
            onClick={() => settrustedAgentModalOpen(true)}
            square
            circle
            size="xl"
            variant="subtle"
          >
            <j-icon size="sm" name="shield-check"></j-icon>
          </j-button>
        </j-tooltip>
        <j-box p="200" />
      </div>
      <div style={{padding: '24px'}}>
        <CardItems 
          title={'Agent ID'}
          value={props.did as string}
          titleUnderline
        />
        <j-box p="200" />
        <CardItems 
          title={'Username'}
          value={profile?.username}
          titleUnderline
        />
        <j-box p="200" />
        <CardItems 
          title={'Name'}
          value={`${profile.firstName} ${profile.lastName}`}
          titleUnderline
        />
        <j-box p="200" />
      </div>
      <j-modal
          size="lg"
          open={trustedAgentModalOpen}
          onToggle={(e: any) => settrustedAgentModalOpen(e.target.open)}
        >
          <j-box p="400">
            <j-flex gap="500" direction="column">
              <j-text nomargin variant="heading-sm">
                Trusted Agents
              </j-text>
              {trustedAgents.map((e, i) => (
                <div key={`trusted-agent-${e.did}`} style={{...cardStyle, marginBottom: 0, width: '85%'}}>
                  <j-flex direction='column' style={{marginTop: 4}}>
                    <j-text weight="bold" >{e?.username || 'No username'}</j-text>
                    <j-flex a="center" j="between">
                      <j-text nomargin variant="body" size="xs">{e?.did.length > 25 ? `${e?.did.substring(0, 25)}...` : e?.did}</j-text>
                      <j-box p="100"></j-box>
                      <j-button size="xs" variant="transparent"  onClick={() => console.log('wow')}>
                        <j-icon size="xs" slot="end" name="clipboard"></j-icon>
                      </j-button>
                    </j-flex>
                  </j-flex>
                </div>
              ))}
            </j-flex>
          </j-box>
      </j-modal>
    </div>
  )
}

export default Profile