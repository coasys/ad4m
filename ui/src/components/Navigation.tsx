import { AppShell, createStyles } from '@mantine/core';
import { useContext, useEffect } from 'react';
import { Grain, Stack2, User, Settings as SettingsIcon } from 'tabler-icons-react';
import { Link, Outlet, useLocation, useNavigate, useRoutes } from 'react-router-dom';
import { Header, RouteContainer } from './styles';
import { Ad4minContext } from '../context/Ad4minContext';
import PackageInfo from '../../package.json'
import Logo from './Logo';
import Profile from './Profile';

type Props = {
  did: String,
  opened: boolean,
  setOpened: (val: boolean) => void
}

const useStyles = createStyles((theme, _params, getRef) => {
  const icon = getRef('icon');
  return {
    header: {
      paddingBottom: theme.spacing.md,
      paddingRight: theme.spacing.md,
      paddingLeft: theme.spacing.md,
      marginBottom: theme.spacing.md * 1.5,
      borderBottom: `1px solid ${theme.colorScheme === 'dark' ? theme.colors.dark[4] : theme.colors.gray[2]}`,
      backgroundColor: 'black'
    },

    link: {
      ...theme.fn.focusStyles(),
      display: 'flex',
      alignItems: 'center',
      margin: '6px 6px 0 6px',
      textDecoration: 'none',
      fontSize: theme.fontSizes.sm,
      color: 'var(--j-color-black)',
      padding: `${theme.spacing.xs}px ${theme.spacing.sm}px`,
      fontWeight: 500,
    },

    linkIcon: {
      ref: icon,
      color: theme.colorScheme === 'dark' ? theme.colors.dark[2] : theme.colors.gray[6],
      marginRight: theme.spacing.sm,
    },

    linkActive: {
      '&, &:hover': {
        borderBottom: 'var(--j-border-width) solid var(--j-color-black)',
        color: 'var(--j-color-black)',
      },
    },

    linkContainer: {
      padding: '0 20px',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'space-between',
      borderBottom: '1px solid var(--j-color-ui-100)'
    }
  };
});

const data = [
  { label: 'Main Settings', link: 'settings', icon: SettingsIcon },
  { label: 'Agent Profile', link: 'profile', icon: User },
  { label: 'Language & Expression', link: 'language', icon: Stack2 },
  { label: 'Perspectives', link: 'perspective', icon: Grain },
]

const Navigation = ({did, opened, setOpened}: Props) => {
  const { classes, cx } = useStyles();
  const {state: {
    connected,
    isUnlocked
  }} = useContext(Ad4minContext);

  let navigate = useNavigate();
  let location = useLocation();

  useEffect(() => {
    if (!connected) {
      navigate('/connect');
    } else if (connected && !isUnlocked) {
      navigate('/login');
    }
}, [connected, isUnlocked, navigate])


  return (
    <AppShell
      padding={0}
      style={{width: '100%'}}
      navbarOffsetBreakpoint="sm"
      asideOffsetBreakpoint="sm"
    >
      <div style={RouteContainer}>
        <div style={Header}>
          <j-flex a="center" j="between">
            <Logo height={30} width={31} />
            <Profile />
          </j-flex>
        </div>
        <div className={classes.linkContainer}>
          <Link to="/language" className={cx(classes.link, { [classes.linkActive]: location.pathname === '/language' })}>
            <j-text variant="caption">Language</j-text>
          </Link>
          <Link to="/perspective" className={cx(classes.link, { [classes.linkActive]: location.pathname === '/perspective' })}>
            <j-text variant="caption">Perspective</j-text>
          </Link>
          <Link to="/settings" className={cx(classes.link, { [classes.linkActive]: location.pathname === '/settings' })}>
            <j-text variant="caption">Settings</j-text>
          </Link>
        </div>
        <Outlet />
      </div>
    </AppShell>
  )
}

export default Navigation