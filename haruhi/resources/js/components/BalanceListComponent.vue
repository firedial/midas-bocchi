<template>
    <div class="container">
        <table class="table table-hover">
            <thead class="thead-light">
                <tr>
                    <th scope="col">Id</th>
                    <th scope="col">Amount</th>
                    <th scope="col">Item</th>
                    <th scope="col">Kind Element</th>
                    <th scope="col">Purpose Element</th>
                    <th scope="col">Place Element</th>
                    <th scope="col">Date</th>
                    <th scope="col">Edit</th>
                </tr>
            </thead>
            <tbody>
                <tr v-for="(balance, index) in balances" :key="index">
                    <th scope="row">{{ balance.id }}</th>
                    <td>{{ balance.amount }}</td>
                    <td>{{ balance.item }}</td>
                    <td>{{ balance.kind_element_description }}</td>
                    <td>{{ balance.purpose_element_description }}</td>
                    <td>{{ balance.place_element_description }}</td>
                    <td>{{ balance.date }}</td>
                    <td>
                        <router-link v-bind:to="{name: 'balance.edit', params: {balanceId: balance.id }}">
                            <button class="btn btn-primary">Edit</button>
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
                balances: []
            }
        },
        methods: {
            getBalances() {
                axios.get('/api/balances?limit=20&orderby=desc')
                    .then((res) => {
                        this.balances = res.data;
                    });
            }
        },
        mounted() {
            this.getBalances();
        }
    }
</script>
