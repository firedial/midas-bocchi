<template>
    <div class="container">
        <table class="table table-hover">
            <thead class="thead-light">
                <tr>
                    <th scope="col">Id</th>
                    <th scope="col">Amount</th>
                    <th scope="col">Item</th>
                    <th scope="col">Before Id</th>
                    <th scope="col">After Id</th>
                    <th scope="col">Date</th>
                    <th scope="col">Edit</th>
                </tr>
            </thead>
            <tbody>
                <tr v-for="(move, index) in moves" :key="index">
                    <th scope="row">{{ move.id }}</th>
                    <td>{{ move.amount }}</td>
                    <td>{{ move.item }}</td>
                    <td>{{ move.before_description }}</td>
                    <td>{{ move.after_description }}</td>
                    <td>{{ move.date }}</td>
                    <td>
                        <router-link v-bind:to="{name: 'move.edit', params: {moveId: move.id, attributeName: attributeName}}">
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
        props: {
            attributeName: String
        },
        data: function () {
            return {
                moves: []
            }
        },
        methods: {
            getMoves() {
                axios.get('/api/moves/' + this.attributeName)
                    .then((res) => {
                        this.moves = res.data;
                    });
            }
        },
        watch: {
            attributeName: function () {
                this.getMoves();
            }
        },
        mounted() {
            this.getMoves();
        }
    }
</script>
