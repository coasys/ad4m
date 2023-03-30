<svelte:options tag={null}/>

<script lang="ts">
    export let commitExpression
    export let discard

    let text = ""

</script>

<div class="container">
    <input bind:value={text}>
    <button on:click={()=>commitExpression(text)}>Commit</button>
    <button on:click={discard}>Discard</button>
</div>


<style>
    .container {
        color: burlywood;
        width: 400px;
        height: 300px;
    }

    input {
        width: 100%;
        height: 200px;
    }
</style>