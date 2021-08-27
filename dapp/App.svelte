<script>
	import Paper, {Title, Subtitle, Content} from '@smui/paper';
	import Button, {Label} from '@smui/button';
	import Textfield from '@smui/textfield'
	import Icon from '@smui/textfield/icon/index';
	import { startEthereum, connectWallet, sign } from './ethereum'
	import Web3 from 'web3'

	let currentAccount = undefined;
	let spinner = false;
	let showSigning = false;

	function handleAccountsChanged(accounts) {
		console.log('Accounts changed: ' + accounts)
		if (accounts.length === 0) {
			// MetaMask is locked or the user has not connected any accounts
			console.log('Please connect to MetaMask.');
		} else if (accounts[0] !== currentAccount && accounts[0]) {
			currentAccount = accounts[0];
		}
	}

	function connect() {
		connectWallet(handleAccountsChanged)
    }
    
    function walletConnectConnect() {
        console.log("Wanting to connect to wallet connect");
    }

    //TODO: get from ad4m
    const staticDid = "did:key:hash";
    function signDid() {
        const { data, signature, r, s, v } = sign(currentAccount, staticDid);
    }

	startEthereum(handleAccountsChanged)
</script>

<main>
    {#if currentAccount==undefined}
        <h2>Get Started</h2>
        <p>
            Welcome to AD4M, thanks for being here. Lets connect all the dapps :)
        <h3> Sign into MetaMask  </h3>
        <Button on:click={connect} variant="raised" color="primary">
            <Label>Connect MetaMask</Label>
        </Button>	
        <br>
        <h3> Sign into WalletConnect </h3>
        <Button on:click={walletConnectConnect} variant="raised" color="primary">
            <Label>Connect WalletConnect</Label>
        </Button>
    {:else}
        <h2>Sign Some Shit</h2>
        <Button on:click={signDid} variant="raised" color="primary">
            <Label>Sign</Label>
        </Button>
    {/if}
</main>

<style>
	main {
		padding: 1em;
		max-width: 240px;
		margin: 0 auto;
		max-width: none;
	}

	h1 {
		color: #ff3e00;
		text-transform: uppercase;
		font-size: 4em;
		font-weight: 100;
	}

	@media (min-width: 640px) {
		main {
			max-width: none;
		}
	}

	@media only screen and (max-width: 600px) {
		main {
			text-align: center;
		}
	}

	.spinner {
		float: right;
		width: 200px;
	}

	.input-icon {
		position: relative;
		margin: 10px
	}
</style>