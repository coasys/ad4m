<svelte:options tag={null}/>

<script lang="ts">
    import type Expression from "@perspect3vism/ad4m/Expression";
    export let expression: Expression
    let string
    $: if(expression && expression.data) string = JSON.parse(expression.data)
</script>

<div class="container">
    {#if expression}
        <input disabled bind:value={string}>
    {/if}
</div>


<style>
    .container {
        color: burlywood;
        width: 400px;
        height: 300px;
    }

    input {
        width: 100%;
        height: 300px;
        color: black;
        background: yellowgreen;
        text-align: center;
        font-size: 20px;
    }
</style>