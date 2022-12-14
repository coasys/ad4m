import { Image } from '@mantine/core';
import { useContext, useEffect, useState } from 'react';
import { AgentContext } from '../context/AgentContext';
import { Ad4minContext } from '../context/Ad4minContext';
import { useNavigate } from 'react-router-dom';


const Login = (props: any) => {
  const {state: {
    loading,
    hasLoginError,
  }, methods: {
    generateAgent,
    unlockAgent,
  }} = useContext(AgentContext)

  const {state: {
    isInitialized,
    isUnlocked,
    connected
  }, methods: {
    resetEndpoint
  }} = useContext(Ad4minContext)
  
  let navigate = useNavigate();

  const [currentIndex, setCurrentIndex] = useState(0);

  const [password, setPassword] = useState("");
  const [username, setUsername] = useState("");
  const [firstName, setFirstName] = useState("");
  const [lastName, setLastName] = useState("");
  const [opened, setOpened] = useState(false);
  const [usernameError, setUsernameError] = useState<string | null>(null);
  let [passwordError, setPasswordError] = useState<string | null>(null);

  if (hasLoginError) {
    passwordError = "Invalid password"
  }

  const onPasswordChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    let { value } = event.target;
    setPassword(value);
  }

  const onFirstNameChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    const { value } = event.target;
    setFirstName(value);
  }

  const onLastNameChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    const { value } = event.target;
    setLastName(value);
  }

  const onUsernameChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    const { value } = event.target;
    setUsername(value);
  }

  const generate = () => {
    if (username.length === 0) {
      setUsernameError("Username is requied");
    } else {
      setUsernameError(null);
    }

    if (password.length === 0) {
      setPasswordError("Password is requied");
    } else {
      setPasswordError(null);
    }

    if (username.length > 0 && password.length > 0) {
      generateAgent(username, firstName, lastName, password);
    }
  }

  // @ts-ignore
  const onKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === "Enter") {
      if(isInitialized) {
        unlockAgent(password);
      } else {
        generate();
      }
    }
  }

  useEffect(() => {
      if (!connected) {
        navigate('/connect');
      } else if (connected && isUnlocked) {
        navigate('/settings');
      }
  }, [connected, isUnlocked, navigate])

  return (
    <div>
      <div className="slider">
        {currentIndex === 0 && <div className="slider__slide">
          <div className="slider__slide-content">
            <j-box pt="500" pb="800">
            <Image style={{width: '200px', margin: 'auto'}} src="ad4msquarelogo2_white_colouremblem.png"></Image>
            </j-box>
            <j-box pt="500">
              <j-text size="800" color="black">
                P2P Framework Beyond Apps
              </j-text>
            </j-box>
            <j-box py="500">
              <j-flex direction="column" gap="200">
                <j-button size="xl" onClick={() => setCurrentIndex(1)} variant="primary">
                  Get Started
                </j-button>
                {!isInitialized ? (<j-button size="lg" variant="link" onClick={() => setCurrentIndex(4)}>
                  Sign up
                </j-button>) : (
                  <j-button size="lg" variant="link" onClick={() => {setCurrentIndex(5)}}>
                  Sign in
                </j-button>
                )}
              </j-flex>
            </j-box>
          </div>
        </div>}
        {currentIndex === 1 && (
          <div className="slider__slide">
            <div className="slider__slide-content">
              <j-box pt="500" pb="800">
              <Image style={{width: '200px', margin: 'auto'}} src="ad4msquarelogo2_white_colouremblem.png"></Image>
              </j-box>
              <j-text variant="heading">
              Privacy and Security
              </j-text>
              <j-text variant="ingress">
              AD4M generates keys on your device, so only you have access to your account and data. No third parties can snoop on your data without your consent.
              </j-text>
              <j-box py="600">
                <j-flex j="center" a="center" gap="500">
                  <j-button variant="link" size="xl" onClick={() => setCurrentIndex(0)}>
                    Previous
                  </j-button>
                  <j-button variant="primary" size="xl" onClick={() => setCurrentIndex(2)}>
                    Next
                  </j-button>
                </j-flex>
              </j-box>
            </div>
          </div>
        )}
        {currentIndex === 2 && (
          <div className="slider__slide">
            <div className="slider__slide-content">
              <j-box pt="500" pb="800">
              <Image style={{width: '200px', margin: 'auto'}} src="ad4msquarelogo2_white_colouremblem.png"></Image>
              </j-box>
              <j-text variant="heading">
              Agent centric
              </j-text>
              <j-text variant="ingress">
              With AD4M you own your data and decide what apps get to use it. No more app silos with you as the central authority.
Censorship free.
              </j-text>
              <j-box py="600">
                <j-flex j="center" a="center" gap="500">
                  <j-button variant="link" size="xl" onClick={() => setCurrentIndex(1)}>
                    Previous
                  </j-button>
                  <j-button variant="primary" size="xl" onClick={() => setCurrentIndex(3)}>
                    Next
                  </j-button>
                </j-flex>
              </j-box>
            </div>
          </div>
        )}
        {currentIndex === 3 && (
          <div className="slider__slide">
            <div className="slider__slide-content">
              <j-box pt="500" pb="800">
              <Image style={{width: '200px', margin: 'auto'}} src="ad4msquarelogo2_white_colouremblem.png"></Image>
              </j-box>
              <j-text variant="heading">
              Censorship free
              </j-text>
              <j-text variant="ingress">
              AD4M allows you to express yourself without fear of censorship or suppression. You can share your thoughts and opinions without worrying about being silenced by a central authority.
              </j-text>
              <j-box py="600">
                <j-flex j="center" a="center" gap="500">
                  <j-button variant="link" size="xl" onClick={() => setCurrentIndex(2)}>
                    Previous
                  </j-button>
                  {!isInitialized ? (<j-button size="lg" variant="primary" onClick={() => setCurrentIndex(4)}>
                  Sign up
                </j-button>) : (
                  <j-button size="lg" variant="primary" onClick={() => {setCurrentIndex(5)}}>
                  Sign in
                </j-button>
                )}
                </j-flex>
              </j-box>
            </div>
          </div>
        )}
        {currentIndex === 4 && (
          <div style={{width: 400, display: 'flex', flexDirection: 'column'}}>
            <j-input
              autofocus
              size="lg"
              label="Username"
              minlength={10}
              maxlength={30}
              autovalidate
              required
              type="text"
              onInput={(e: any) => setUsername(e.target.value)}
            ></j-input>
            <j-box p="400"></j-box>
            <j-input
              autofocus
              size="lg"
              label="Firstname"
              minlength={10}
              maxlength={30}
              autovalidate
              required
              type="text"
              onInput={(e: any) => setFirstName(e.target.value)}
            ></j-input>
            <j-box p="400"></j-box>
            <j-input
              autofocus
              size="lg"
              label="Lastname"
              minlength={10}
              maxlength={30}
              autovalidate
              required
              type="password"
              onInput={(e: any) => setLastName(e.target.value)}
            ></j-input>
            <j-box p="400"></j-box>
            <j-input
              autofocus
              size="lg"
              label="Password"
              minlength={10}
              maxlength={30}
              autovalidate
              required
              type="password"
              onInput={(e: any) => setPassword(e.target.value)}
            ></j-input>
            <j-box p="400"></j-box>
            <j-button size="lg" variant="primary" style={{alignSelf: 'center'}} onClick={() => generate()} loading={loading}>
              Generate Agent
            </j-button>
          </div>
        )}
        {currentIndex === 5 && (
          <div style={{width: 400, display: 'flex', flexDirection: 'column'}}>
            <j-input
              autofocus
              size="lg"
              label="Password"
              minlength={10}
              maxlength={30}
              autovalidate
              required
              type="password"
              full
              onInput={(e: any) => setPassword(e.target.value)}
            ></j-input>
            <j-box p="400"></j-box>
            <j-button 
              size="lg" 
              variant="primary" 
              style={{alignSelf: 'center'}} 
              onClick={() => unlockAgent(password)} 
              loading={loading}>
              Unlock Agent
            </j-button>
          </div>
        )}
      </div>
    </div>
  )
}

export default Login