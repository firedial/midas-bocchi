<template>
    <div class="container">
        <form v-on:submit.prevent="submit">
            <div class="form-group row">
                <label for="email" class="col-sm-3 col-form-label">Email</label>
                <input type="text" class="col-sm-3 col-form-label" id="email" v-model=email>
            </div>
            <div class="form-group row">
                <label for="password" class="col-sm-3 col-form-label">Password</label>
                <input type="password" class="col-sm-3 col-form-label" id="passoword" v-model=password>
            </div>
            <button type="submit" class="btn btn-primary">Submit</button>
        </form>
    </div>
</template>

<script>
export default {
    data: function () {
        return {
            email: '',
            password: '',
        }
    },
    methods: {
        submit() {
            axios.get('/sanctum/csrf-cookie')
                .then(() => {
                    axios.post('/api/login', { email: this.email, password: this.password })
                });
        },
    },
}
</script>