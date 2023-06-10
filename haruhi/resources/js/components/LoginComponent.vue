<template>
    <div class="container">
        <table class="table table-hover">
            <thead class="thead-light">
                <tr>
                    <th scope="col">項目</th>
                    <th scope="col">リンク</th>
                </tr>
            </thead>
            <tbody>
                <tr v-for="(link, index) in links" :key="index">
                    <td>
                        {{ link.name }}
                    </td>
                    <td>
                        <router-link v-bind:to="{ name: link.url, params: { attributeName: link.attributeName } }">
                            <button class="btn btn-primary">{{ link.value }}</button>
                        </router-link>
                    </td>
                </tr>
            </tbody>
        </table>
    </div>
</template>

<script>
export default {
    data: function () {
        return {
        }
    },
    methods: {
        getKindElements() {
            axios.get('/sanctum/csrf-cookie')
                .then(() => {
                    axios.post('/api/login', { email: "hoge@example.com", password: "fa" })
                });
        },
    },
    mounted() {
        this.getKindElements();
    }
}
</script>