import VueRouter from 'vue-router';
import HeaderComponent from "./components/HeaderComponent";
import BalanceListComponent from "./components/BalanceListComponent";
import BalanceCreateComponent from "./components/BalanceCreateComponent";
import BalanceEditComponent from "./components/BalanceEditComponent";
import BalanceShowComponent from "./components/BalanceShowComponent";
import MoveListComponent from "./components/MoveListComponent";
import MoveCreateComponent from "./components/MoveCreateComponent";
import MoveEditComponent from "./components/MoveEditComponent";
import MoveShowComponent from "./components/MoveShowComponent";
import SalaryCreateComponent from "./components/SalaryCreateComponent";
import BonusCreateComponent from "./components/BonusCreateComponent";
import MonthlyCreateComponent from "./components/MonthlyCreateComponent";

/**
 * First we will load all of this project's JavaScript dependencies which
 * includes Vue and other libraries. It is a great starting point when
 * building robust, powerful web applications using Vue and Laravel.
 */

require('./bootstrap');

window.Vue = require('vue').default;

Vue.use(VueRouter);

const router = new VueRouter({
    mode: 'history',
    routes: [
        {
            path: '/balances',
            name: 'balance.list',
            component: BalanceListComponent
            // component: BalanceTableComponent
        },
        {
            path: '/balances/create',
            name: 'balance.create',
            component: BalanceCreateComponent
        },
        {
            path: '/balances/:balanceId/edit',
            name: 'balance.edit',
            component: BalanceEditComponent,
            props: true
        },
        {
            path: '/balances/:balanceId',
            name: 'balance.show',
            component: BalanceShowComponent,
            props: true
        },
        {
            path: '/moves/:attributeName',
            name: 'move.list',
            component: MoveListComponent,
            props: true
        },
        {
            path: '/moves/:attributeName/create',
            name: 'move.create',
            component: MoveCreateComponent,
            props: true
        },
        {
            path: '/moves/:attributeName/:moveId/edit',
            name: 'move.edit',
            component: MoveEditComponent,
            props: true
        },
        {
            path: '/moves/:attributeName/:moveId',
            name: 'move.show',
            component: MoveShowComponent,
            props: true
        },
        {
            path: '/salary/create',
            name: 'salary.create',
            component: SalaryCreateComponent
        },
        {
            path: '/bonus/create',
            name: 'bonus.create',
            component: BonusCreateComponent
        },
        {
            path: '/monthly/create',
            name: 'monthly.create',
            component: MonthlyCreateComponent
        },
    ]
});

/**
 * The following block of code may be used to automatically register your
 * Vue components. It will recursively scan this directory for the Vue
 * components and automatically register them with their "basename".
 *
 * Eg. ./components/ExampleComponent.vue -> <example-component></example-component>
 */

// const files = require.context('./', true, /\.vue$/i)
// files.keys().map(key => Vue.component(key.split('/').pop().split('.')[0], files(key).default))

Vue.component('example-component', require('./components/ExampleComponent.vue').default);
Vue.component('header-component', HeaderComponent);

/**
 * Next, we will create a fresh Vue application instance and attach it to
 * the page. Then, you may begin adding components to this application
 * or customize the JavaScript scaffolding to fit your unique needs.
 */

const app = new Vue({
    el: '#app',
    router
});
