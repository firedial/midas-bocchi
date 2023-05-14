<template>
    <div class="container">
        <div class="row justify-content-center">
            <div class="col-sm-7">
                <form v-on:submit.prevent="submit" autocomplete="off">
                    <div class="form-group row">
                        <label for="base_salary" class="col-sm-3 col-form-label">base salary</label>
                        <input type="text" class="col-sm-6 form-control" id="base_salary" v-model="salary.baseSalary">
                    </div>
                    <div class="form-group row">
                        <label for="adjustment_salary" class="col-sm-3 col-form-label">adjustment salary</label>
                        <input type="text" class="col-sm-6 form-control" id="adjustment_salary" v-model="salary.adjustmentSalary">
                    </div>
                    <div class="form-group row">
                        <label for="transportation" class="col-sm-3 col-form-label">transportation</label>
                        <input type="text" class="col-sm-6 form-control" id="transportation" v-model="salary.transportation">
                    </div>
                    <div class="form-group row">
                        <label for="holding_incentives" class="col-sm-3 col-form-label">holding incentives</label>
                        <input type="text" class="col-sm-6 form-control" id="amount" v-model="salary.holdingIncentives">
                    </div>
                    <div class="form-group row">
                        <label for="health_insurance" class="col-sm-3 col-form-label">health insurance</label>
                        <input type="text" class="col-sm-6 form-control" id="health_insurance" v-model="salary.healthInsurance">
                    </div>
                    <div class="form-group row">
                        <label for="welfare_pension" class="col-sm-3 col-form-label">welfare pension</label>
                        <input type="text" class="col-sm-6 form-control" id="welfare_pension" v-model="salary.welfarePension">
                    </div>
                    <div class="form-group row">
                        <label for="resident_tax" class="col-sm-3 col-form-label">resident tax</label>
                        <input type="text" class="col-sm-6 form-control" id="resident_tax" v-model="salary.residentTax">
                    </div>
                    <div class="form-group row">
                        <label for="employment_insurance" class="col-sm-3 col-form-label">employment insurance</label>
                        <input type="text" class="col-sm-6 form-control" id="employment_insurance" v-model="salary.employmentInsurance">
                    </div>
                    <div class="form-group row">
                        <label for="income_tax" class="col-sm-3 col-form-label">income tax</label>
                        <input type="text" class="col-sm-6 form-control" id="income_tax" v-model="salary.incomeTax">
                    </div>
                    <div class="form-group row">
                        <label for="holding" class="col-sm-3 col-form-label">holding</label>
                        <input type="text" class="col-sm-6 form-control" id="holding" v-model="salary.holding">
                    </div>
                    <div class="form-group row">
                        <label for="date" class="col-sm-3 col-form-label">date</label>
                        <input type="date" class="col-sm-6 form-control" id="date" v-model="salary.date">
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
                salary: {},
                response: {
                    isReturned: false,
                    isSucceeded: false,
                    returnMessage: ''
                }
            }
        },
        methods: {
            submit() {
                this.response.isReturned = false;
                axios.post('/api/salary', this.salary)
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
        }
    }
</script>

