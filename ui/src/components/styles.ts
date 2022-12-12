export const MainContainer = {
  width: 'calc(100%)',
  position: 'relative' as 'relative',
  padding: 0,
  maxWidth: '100%',
  overflowX: 'clip' as 'clip',
  margin: 0
}

export const MainHeader = {
  width: 'calc(100% - 40px)',
  background: '#000',
  display: 'flex',
  justifyContent: 'space-between',
  padding: `20px`,
  fontFamily: 'comfortaa'
}

export const RouteContainer = { 
  width: '100%', 
  overflowX: 'hidden' as 'hidden',
  maxWidth: 'calc(100%-320px)'
}

export const Header = {
  padding: '10px'
}

export const cardStyle = {
  width: '100%',
  display: 'flex',
  alignItems: 'center',
  gap: 'var(--j-space-700)',
  borderRadius: 'var(--j-border-radius)',
  textDecoration: 'none',
  backgroundColor: 'var(--j-color-ui-50)',
  padding: 'var(--j-space-500)',
  marginBottom: 'var(--j-space-700)'
}

export const listStyle = {
  overflow: 'scroll',
  padding: '20px 20px',
  height: '50%'
}

export const gridButton = {
  display: 'grid',
  columnGap: 'calc(100% - 312px)',
  gridTemplateColumns: 'repeat(4, 52px)',
  justifyContent: 'center',
  alignItems: 'center',
  padding: '10px 20px 0 20px'
}