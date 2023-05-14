<template>
    <div class="container">
        <table class="table table-hover">
            <thead class="thead-light">
                <tr>
                    <th>name</th>
                    <th v-for="(column, index) in data.labels" :key="index">{{ column }}</th>
                </tr>
            </thead>
            <tbody>
                <tr v-for="(row, index) in data.datasets" :key="index">
                    <th scope="row">{{ row.label }}</th>
                    <td v-for="(value, index) in row.data" :key="index">{{ value }}</td>
                </tr>
            </tbody>
        </table>
    </div>
</template>

<script>
    export default {
        props: {
            data: Object
        }
    }
</script>
