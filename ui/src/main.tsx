import { render } from 'preact';
import App from './App';
import reportWebVitals from './reportWebVitals';
import { MantineProvider } from '@mantine/core';
import { NotificationsProvider } from '@mantine/notifications';
import { Ad4minProvider } from './context/Ad4minContext';
import { BrowserRouter as Router} from 'react-router-dom';
import "@junto-foundation/junto-elements";
import "@junto-foundation/junto-elements/dist/main.css";
import './index.css';


const Main = () => {
  return (
    <MantineProvider>
    <NotificationsProvider>
      <Ad4minProvider>
        <Router>
          <App />
        </Router>
      </Ad4minProvider>
    </NotificationsProvider>
  </MantineProvider>
  )
}


render(
  <Main />,
document.getElementById('root')
)

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
