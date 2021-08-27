<script>
</script>

<main>
    <div>
        <h1>Hello World!</h1>
    </div>
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