<template>
    <div class="container">
        <table class="table table-hover">
            <thead class="thead-light">
                <tr>
                    <th scope="col">Id</th>
                    <th scope="col">Name</th>
                    <th scope="col">Description</th>
                    <th scope="col">Category Id</th>
                    <th scope="col">Edit</th>
                    <th scope="col">Save</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <th>+</th>
                    <td><input type="text" class="col-sm-9 form-control" v-model="newAttributeElement.name"></td>
                    <td><input type="text" class="col-sm-9 form-control" v-model="newAttributeElement.description"></td>
                    <td>
                        <select class="form-select form-select-sm" v-model="newAttributeElement.category_id">
                            <option value=""></option>
                            <option v-for="attributeCategory in attributeCategories" :value="attributeCategory.id"
                                :key="attributeCategory.id">
                                {{ attributeCategory.name }}
                            </option>
                        </select>
                    </td>
                    <td>
                        <button class="btn btn-success" v-on:click="clearNewAttributeElement()">Cancel</button>
                    </td>
                    <td>
                        <button class="btn btn-primary" v-on:click="addNewAttributeElement()">Save</button>
                    </td>
                </tr>
                <tr v-for="(attributeElement, index) in attributeElements" :key="index">
                    <th scope="row">{{ attributeElement.id }}</th>
                    <td><input type="text" class="col-sm-9 form-control" v-bind:readonly="isReadOnly(attributeElement.id)"
                            v-model="attributeElement.name"></td>
                    <td><input type="text" class="col-sm-9 form-control" v-bind:readonly="isReadOnly(attributeElement.id)"
                            v-model="attributeElement.description"></td>
                    <td v-if="isReadOnly(attributeElement.id)"><input type="text" class="col-sm-9 form-control" readonly
                            v-model="attributeElement.category_id"></td>
                    <td v-else>
                        <select class="form-select form-select-sm" v-bind:disabled="isReadOnly(attributeElement.id)"
                            v-model="attributeElement.category_id">
                            <option value=""></option>
                            <option v-for="attributeCategory in attributeCategories" :value="attributeCategory.id"
                                :key="attributeCategory.id">
                                {{ attributeCategory.description }}
                            </option>
                        </select>
                    </td>
                    <td v-if="isReadOnly(attributeElement.id)">
                        <button class="btn btn-success" v-on:click="editAttributeElement(attributeElement.id)">Edit</button>
                    </td>
                    <td v-else>
                        <button class="btn btn-success" v-on:click="cancelEditAttributeElement()">Cancel</button>
                    </td>
                    <td>
                        <button class="btn btn-primary" :disabled="isReadOnly(attributeElement.id)"
                            v-on:click="saveAttributeElement(attributeElement)">Save</button>
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
            attributeElements: [],
            attributeCategories: [],
            newAttributeElement: {},
            editable: null
        }
    },
    methods: {
        getAttributeElements() {
            axios.get('/api/attribute_elements/' + this.attributeName + '_element')
                .then((res) => {
                    this.attributeElements = res.data;
                });
        },
        getAttributeCategories() {
            axios.get('/api/attribute_categories/' + this.attributeName + '_category')
                .then((res) => {
                    this.attributeCategories = res.data;
                });
        },
        editAttributeElement(id) {
            this.editable = id;
        },
        isReadOnly(id) {
            return this.editable !== id;
        },
        saveAttributeElement(attributeElement) {
            axios.put('/api/attribute_elements/' + this.attributeName + '_element/' + attributeElement.id, attributeElement)
                .then((res) => {
                    if (res.status === 200) {
                        this.editable = null;
                    }
                });
        },
        cancelEditAttributeElement() {
            this.editable = null;
        },
        clearNewAttributeElement() {
            this.newAttributeElement = {};
        },
        addNewAttributeElement() {
            axios.post('/api/attribute_elements/' + this.attributeName + '_element', this.newAttributeElement)
                .then((res) => {
                    if (res.status === 200) {
                        this.newAttributeElement = {};
                        this.attributeElements.push(res.data);
                        this.clearNewAttributeElement()
                    }
                });
        },
    },
    mounted() {
        this.getAttributeElements();
        this.getAttributeCategories();
    }
}
</script>
