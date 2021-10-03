<script>
	import Paper, {Title, Subtitle, Content} from '@smui/paper';
	import Button, {Label} from '@smui/button';
	import Textfield from '@smui/textfield'
    import Icon from '@smui/textfield/icon/index';
    import { NotificationDisplay, notifier } from '@beyonk/svelte-notifications'
	import { startEthereum, connectWallet, sign } from './ethereum'
    import Web3 from 'web3'
    import {
        ApolloClient,
        InMemoryCache
    } from "@apollo/client/core";
    import { gql } from "@apollo/client";
    import { WebSocketLink } from '@apollo/client/link/ws';
    import { Ad4mClient } from "@perspect3vism/ad4m";

	let currentAccount = undefined;
	let spinner = false;
    let showSigning = false;
    let password = undefined;

    function constructApolloClient(port) {
        return new ApolloClient({
            link: new WebSocketLink({
                uri: `ws://localhost:${port}/graphql`,
                options: { reconnect: true },
            }),
            cache: new InMemoryCache(),
        });
    }
    const apolloClient = constructApolloClient(4000)
    const ad4mClient = new Ad4mClient(apolloClient)

    let signedIn = false;
    let did = null;

    ad4mClient.agent.status().then(result => {
        signedIn = result.isUnlocked;
        did = result.did;
    })

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

    function signDid() {
        //TODO: check that EP has not already been created with current eth address
        sign(currentAccount, did).then(result => {
            const { data, signature, r, s, v } = result;
            //Then sign eth address with did
            ad4mClient.agent.entanglementProofPreFlight(ethereum.selectedAddress).then(result => {
                //Add the signed did to the proof
                result.didSignedByDeviceKey = signature;
                delete result["__typename"];
                console.log("Sending", result);
                //Save proof into ad4m
                ad4mClient.agent.addEntanglementProofs([result]).then(result => {
                    console.log("Added EP with result", result);
                    notifier.success('Ethereum Proof Generated!');
                })
            })
        });
    }

    function unlock() {
        ad4mClient.agent.unlock(password).then(result => {
            console.log("Got unlock result", result);
            if (result.isUnlocked == true) {
                signedIn = true;
            } else {
                signedIn = false;
                notifier.error('Incorrect Password!')
            }
        })
    }

	startEthereum(handleAccountsChanged)
</script>

<main>
    <NotificationDisplay></NotificationDisplay>
    {#if signedIn==true}
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
            <h2>Create link</h2>
            <Button on:click={signDid} variant="raised" color="primary">
                <Label>Sign</Label>
            </Button>
        {/if}
    {:else}
        <h2>Please unlock your ad4m agent...</h2>
        <br>
        <Label>Password:</Label>
        <input bind:value={password} variant="raised" color="primary" type="password">
        <Button on:click={unlock} variant="raised" color="primary">
            <Label>Unlock</Label>
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