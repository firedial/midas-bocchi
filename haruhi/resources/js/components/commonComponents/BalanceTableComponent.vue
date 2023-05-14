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
                </tr>
            </tbody>
        </table>
    </div>
</template>

<script>
    export default {
        props: {
            balances: Array
        }
    }
</script>
