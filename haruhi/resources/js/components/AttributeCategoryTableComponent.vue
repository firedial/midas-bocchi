<template>
    <div class="container">
        <table class="table table-hover">
            <thead class="thead-light">
                <tr>
                    <th scope="col">Id</th>
                    <th scope="col">Name</th>
                    <th scope="col">Description</th>
                    <th scope="col">Edit</th>
                    <th scope="col">Save</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <th>+</th>
                    <td><input type="text" class="col-sm-9 form-control" v-model="newAttributeCategory.name"></td>
                    <td><input type="text" class="col-sm-9 form-control" v-model="newAttributeCategory.description"></td>
                    <td>
                        <button class="btn btn-success" v-on:click="clearNewAttributeCategory()">Cancel</button>
                    </td>
                    <td>
                        <button class="btn btn-primary" v-on:click="addNewAttributeCategory()">Save</button>
                    </td>
                </tr>
                <tr v-for="(attributeCategory, index) in attributeCategories" :key="index">
                    <th scope="row">{{ attributeCategory.id }}</th>
                    <td><input type="text" class="col-sm-9 form-control" v-bind:readonly="isReadOnly(attributeCategory.id)"
                            v-model="attributeCategory.name"></td>
                    <td><input type="text" class="col-sm-9 form-control" v-bind:readonly="isReadOnly(attributeCategory.id)"
                            v-model="attributeCategory.description"></td>
                    <td v-if="isReadOnly(attributeCategory.id)">
                        <button class="btn btn-success"
                            v-on:click="editAttributeCategory(attributeCategory.id)">Edit</button>
                    </td>
                    <td v-else>
                        <button class="btn btn-success" v-on:click="cancelEditAttributeCategory()">Cancel</button>
                    </td>
                    <td>
                        <button class="btn btn-primary" :disabled="isReadOnly(attributeCategory.id)"
                            v-on:click="saveAttributeCategory(attributeCategory)">Save</button>
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
            attributeCategories: [],
            newAttributeCategory: {},
            editable: null
        }
    },
    methods: {
        getAttributeCategories() {
            axios.get('/api/attribute_categories/' + this.attributeName + '_category')
                .then((res) => {
                    this.attributeCategories = res.data;
                });
        },
        editAttributeCategory(id) {
            this.editable = id;
        },
        isReadOnly(id) {
            return this.editable !== id;
        },
        saveAttributeCategory(attributeCategory) {
            axios.put('/api/attribute_categories/' + this.attributeName + '_category/' + attributeCategory.id, attributeCategory)
                .then((res) => {
                    if (res.status === 200) {
                        this.editable = null;
                    }
                });
        },
        cancelEditAttributeCategory() {
            this.editable = null;
        },
        clearNewAttributeCategory() {
            this.newAttributeCategory = {};
        },
        addNewAttributeCategory() {
            axios.post('/api/attribute_categories/' + this.attributeName + '_category', this.newAttributeCategory)
                .then((res) => {
                    if (res.status === 200) {
                        this.attributeCategories.push(res.data);
                        this.clearNewAttributeCategory()
                    }
                });
        },
    },
    mounted() {
        this.getAttributeCategories();
    }
}
</script>
