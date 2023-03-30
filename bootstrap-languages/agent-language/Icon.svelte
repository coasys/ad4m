<svelte:options tag={null}/>

<script lang="ts">
    import type { Expression } from "@perspect3vism/ad4m";
    import { Literal, Perspective } from "@perspect3vism/ad4m";
    import emailValidator from 'email-validator'
    import md5 from 'md5'

    export let expression: Expression
    let did
    let email
    let firstName
    let lastName

    async function update() {
        let perspective = new Perspective(expression.data.perspective.links)
        did = expression.data.did

        try {
            firstName = Literal.fromUrl(await perspective.getSingleTarget({source: did, predicate: 'foaf://givenName'})).get()
        }catch(e) {
            firstName = undefined
        }

        try {
            lastName = Literal.fromUrl(await perspective.getSingleTarget({source: did, predicate: 'foaf://familyName'})).get()
        }catch(e) {
            lastName = undefined
        }

        try {
            email = Literal.fromUrl(await perspective.getSingleTarget({source: did, predicate: 'foaf://mbox'})).get()
        }catch(e) {
            email = undefined
        }
    }

    $: if(expression) update()

</script>

<div class="container">
    
    <div class="text">
        <h1>{did}</h1>
        <h2>{firstName} {lastName}</h2>
        <h2>{email}</h2>
    </div>
    
    {#if emailValidator.validate(email) }
        <img src="http://www.gravatar.com/avatar/{md5(email)}?s=360" alt="gravatar" class="image">
    {/if}
</div>


<style>
    h1 {
        width: 100%;
        word-break: break-all;
    }
    .container {
        color: white;
        width: 400px;
        height: 300px;
        text-shadow: black;
    }

    .text {
        position: absolute;
        left: 0;
        right: 0;
        top: 0;
        bottom: 0;
        z-index: 3;
    } 
    .image {
        position: absolute;
        left: 0;
        right: 0;
        top: 0;
        bottom: 0;
    } 
</style>