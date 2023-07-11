<svelte:options tag={null}/>

<script lang="ts">
    import type Expression from "@perspect3vism/ad4m/Expression";
    export let expression: Expression
    let perspectiveExpression
    $: if(expression && expression.data) perspectiveExpression = JSON.parse(expression.data)
</script>

<div class="container">
    {#if expression}
        <p>{JSON.stringify(perspectiveExpression.links)}</p>
    {/if}
</div>


<style>
    .container {
        color: burlywood;
        width: 400px;
        height: 300px;
    }

    p {
        width: 100%;
        height: 300px;
        color: black;
        background: yellowgreen;
        text-align: center;
        font-size: 20px;
    }
</style>