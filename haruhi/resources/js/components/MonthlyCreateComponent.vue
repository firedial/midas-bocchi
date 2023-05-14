<template>
    <div class="container">
        <div class="row justify-content-center">
            <div class="col-sm-7">
                <form v-on:submit.prevent="submit" autocomplete="off">
                    <div v-for="item in items" class="form-group row">
                        <label v-bind:for="item" class="col-sm-3 col-form-label">{{item}}</label>
                        <input type="text" class="col-sm-4 form-control" v-bind:id="item + '_amount'" v-model="data[item].amount">

                        <input type="date" class="col-sm-3 form-control" v-bind:id="item + '_date'" v-model="data[item].date">
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
                items: ['house_rent', 'gas', 'water', 'elect', 'net'],
                data: {},
                response: {
                    isReturned: false,
                    isSucceeded: false,
                    returnMessage: ''
                }
            }
        },
        methods: {
            init() {
                for (let i = 0; i < this.items.length; i++) {
                    this.data[this.items[i]] = {'amount': 0, 'date': ''}
                }
                console.log(this.data)
            },
            submit() {
                this.response.isReturned = false;
                axios.post('/api/monthly', this.data)
                    .then((res) => {
                        this.response.isReturned = true;
                        this.response.isSucceeded = true;
                        this.salary = {};
                    }).catch((error) => {
                        this.response.isReturned = true;
                        this.response.isSucceeded = false;
                        this.response.returnMessage = error.response.data.message;
                    });
            }
        },
        beforeMount() {
            this.init()
        }
    }
</script>

