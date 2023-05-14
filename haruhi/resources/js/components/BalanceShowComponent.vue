<template>
    <div class="container">
        <div class="row justify-content-center">
            <div class="col-sm-6">
                <form>
                    <div class="form-group row">
                        <label for="id" class="col-sm-3 col-form-label">Id</label>
                        <input type="text" class="col-sm-9 form-control-plaintext" readonly id="id" v-model="balance.id">
                    </div>
                    <div class="form-group row">
                        <label for="amount" class="col-sm-3 col-form-label">Amount</label>
                        <input type="text" class="col-sm-9 form-control-plaintext" readonly id="amount" v-model="balance.amount">
                    </div>
                    <div class="form-group row">
                        <label for="item" class="col-sm-3 col-form-label">Item</label>
                        <input type="text" class="col-sm-9 form-control-plaintext" readonly id="item" v-model="balance.item">
                    </div>
                    <div class="form-group row">
                        <label for="kind_element" class="col-sm-3 col-form-label">Kind Element</label>
                        <input type="text" class="col-sm-9 form-control-plaintext" readonly id="kind_element" v-model="balance.kind_element_id">
                    </div>
                    <div class="form-group row">
                        <label for="purpose_element" class="col-sm-3 col-form-label">Purpose Element</label>
                        <input type="text" class="col-sm-9 form-control-plaintext" readonly id="purpose_element" v-model="balance.purpose_element_id">
                    </div>
                    <div class="form-group row">
                        <label for="place_element" class="col-sm-3 col-form-label">Place Element</label>
                        <input type="text" class="col-sm-9 form-control-plaintext" readonly id="place_element" v-model="balance.place_element_id">
                    </div>
                    <div class="form-group row">
                        <label for="date" class="col-sm-3 col-form-label">Date</label>
                        <input type="text" class="col-sm-9 form-control-plaintext" readonly id="date" v-model="balance.date">
                    </div>
                </form>
            </div>
        </div>
    </div>
</template>

<script>
    export default {
        props: {
            balanceId: String
        },
        data: function () {
            return {
                balance: {}
            }
        },
        methods: {
            getBalance() {
                axios.get('/api/balances/' + this.balanceId)
                    .then((res) => {
                        this.balance = res.data;
                    });
            }
        },
        mounted() {
            this.getBalance();
        }
    }
</script>

