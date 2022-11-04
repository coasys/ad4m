import { PasswordInput, Button, Stack, TextInput, ActionIcon, Tooltip } from '@mantine/core';
import { useContext, useEffect, useState } from 'react';
import { AgentContext } from '../context/AgentContext';
import { Link } from 'tabler-icons-react';
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
        navigate('/profile');
      }
  }, [connected, isUnlocked, navigate])

  return (
    <div>
      <Stack align="center" spacing="xl">
        <div style={{
          position: 'fixed',
          top: 20,
          right: 20
        }}>
          <Tooltip
            label="Change ad4m-executor url"
            opened={opened}
            position="left"
            placement="center"
            withArrow
          >
            <ActionIcon 
              onMouseEnter={() => setOpened(true)} 
              onMouseLeave={() => setOpened(false)}
              onClick={() => resetEndpoint()}
            >
              <Link color={opened ? '#000' : '#fff'} />
            </ActionIcon >
          </Tooltip>
        </div>
        <div style={{ width: 280 }}>
          {!isInitialized && (
            <>
              <TextInput 
                label="Username" 
                placeholder='satoshi_01' 
                radius="md" 
                size="md"
                required
                error={usernameError}
                onChange={onUsernameChange}
              />
              <TextInput 
                label="First Name" 
                placeholder='Satoshi' 
                radius="md" 
                size="md" 
                onChange={onFirstNameChange}
              />
              <TextInput 
                label="Last Name" 
                placeholder='Nakamoto' 
                radius="md" 
                size="md" 
                onChange={onLastNameChange}
              />
            </>)
          }
          <PasswordInput
            placeholder="Password"
            label="Input your passphrase"
            radius="md"
            size="md"
            required
            error={passwordError}
            onChange={onPasswordChange}
            onKeyDown={onKeyDown}
          />
        </div>

        {
          !isInitialized &&
          <Button onClick={() => generate()} loading={loading}>
            Generate agent
          </Button>
        }
        {
          isInitialized && !isUnlocked &&
          <Button onClick={() => unlockAgent(password)} loading={loading}>
            Unlock agent
          </Button>
        }
      </Stack>
    </div>
  )
}

export default Login