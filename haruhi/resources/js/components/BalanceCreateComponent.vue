<template>
    <div class="container">
        <div class="row justify-content-center">
            <div class="col-sm-7">
                <form v-on:submit.prevent="submit">
                    <div class="form-group row">
                        <label for="amount" class="col-sm-3 col-form-label">Amount</label>
                        <input type="text" class="col-sm-6 form-control" id="amount" v-model="balance.amount">
                        <input type="checkbox" class="col-sm-1 form-control" id="amount_checkbox" v-model="balanceLeave.amount">
                    </div>
                    <div class="form-group row">
                        <label for="item" class="col-sm-3 col-form-label">Item</label>
                        <input type="text" class="col-sm-6 form-control" id="item" v-model="balance.item">
                        <input type="checkbox" class="col-sm-1 form-control" id="item_checkbox" v-model="balanceLeave.item">
                    </div>
                    <div class="form-group row">
                        <label for="kind_element" class="col-sm-3 col-form-label">Kind Element</label>
                        <select class="form-select col-sm-6" v-model="balance.kind_element_id">
                            <option value=""></option>
                            <option v-for="kindElement in kindElements" :value="kindElement.id" :key="kindElement.id">
                                {{ kindElement.description }}
                            </option>
                        </select>
                        <input type="checkbox" class="col-sm-1 form-control" id="kind_element_id_checkbox" v-model="balanceLeave.kindElementId">
                    </div>
                    <div class="form-group row">
                        <label for="purpose_element" class="col-sm-3 col-form-label">Purpose Element</label>
                        <select class="form-select col-sm-6" v-model="balance.purpose_element_id">
                            <option value=""></option>
                            <option v-for="purposeElement in purposeElements" :value="purposeElement.id" :key="purposeElement.id">
                                {{ purposeElement.description }}
                            </option>
                        </select>
                        <input type="checkbox" class="col-sm-1 form-control" id="purpose_element_id_checkbox" v-model="balanceLeave.purposeElementId">
                    </div>
                    <div class="form-group row">
                        <label for="place_element" class="col-sm-3 col-form-label">Place Element</label>
                        <select class="form-select col-sm-6" v-model="balance.place_element_id">
                            <option value=""></option>
                            <option v-for="placeElement in placeElements" :value="placeElement.id" :key="placeElement.id">
                                {{ placeElement.description }}
                            </option>
                        </select>
                        <input type="checkbox" class="col-sm-1 form-control" id="place_element_id_checkbox" v-model="balanceLeave.placeElementId">
                    </div>
                    <div class="form-group row">
                        <label for="date" class="col-sm-3 col-form-label">Date</label>
                        <input type="date" class="col-sm-6 form-control" id="date" v-model="balance.date">
                        <input type="checkbox" class="col-sm-1 form-control" id="date_checkbox" v-model="balanceLeave.date">
                    </div>
                    <button type="submit" class="btn btn-primary">Submit</button>
                    <div class="form-group row">
                        <div v-if="response.isReturned">
                            <div v-if="response.isSucceeded" class="alert alert-success">
                                Succeeded!
                            </div>
                            <div v-else class="alert alert-danger">
                                {{ response.returnMessage }}
                            </div>
                        </div>
                    </div>
                </form>
            </div>
        </div>
    </div>
</template>

<script>
    export default {
        data: function () {
            return {
                balance: {},
                balanceLeave: {
                    amount: false,
                    item: false,
                    kineElementId: false,
                    purposeElementId: false,
                    placeElementId: false,
                    date: false
                },
                kindElements: [],
                purposeElements: [],
                placeElements: [],
                response: {
                    isReturned: false,
                    isSucceeded: false,
                    returnMessage: ''
                }
            }
        },
        methods: {
            getKindElements() {
                axios.get('/api/attribute_elements/kind_element')
                    .then((res) => {
                        this.kindElements = res.data;
                    });
            },
            getPurposeElements() {
                axios.get('/api/attribute_elements/purpose_element')
                    .then((res) => {
                        this.purposeElements = res.data;
                    });
            },
            getPlaceElements() {
                axios.get('/api/attribute_elements/place_element')
                    .then((res) => {
                        this.placeElements = res.data;
                    });
            },
            flushBalance() {
                if (!this.balanceLeave.amount) {
                    this.balance.amount = undefined;
                }
                if (!this.balanceLeave.item) {
                    this.balance.item = undefined;
                }
                if (!this.balanceLeave.kindElementId) {
                    this.balance.kind_element_id = undefined;
                }
                if (!this.balanceLeave.purposeElementId) {
                    this.balance.purpose_element_id = undefined;
                }
                if (!this.balanceLeave.placeElementId) {
                    this.balance.place_element_id = undefined;
                }
                if (!this.balanceLeave.date) {
                    this.balance.date = undefined;
                }
            },
            submit() {
                this.response.isReturned = false;
                axios.post('/api/balances', this.balance)
                    .then((res) => {
                        this.response.isReturned = true;
                        this.response.isSucceeded = true;
                        this.flushBalance();
                    }).catch((error) => {
                        this.response.isReturned = true;
                        this.response.isSucceeded = false;
                        this.response.returnMessage = error.response.data.message;
                    });
            }
        },
        mounted() {
            this.getKindElements();
            this.getPurposeElements();
            this.getPlaceElements();
        }
    }
</script>

