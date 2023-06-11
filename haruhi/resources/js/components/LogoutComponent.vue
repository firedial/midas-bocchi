<template>
    <div class="container">
    </div>
</template>

<script>
export default {
    methods: {
        logout() {
            axios.post('/api/logout')
        },
    },
    mounted() {
        this.logout()
    },
}
</script>
