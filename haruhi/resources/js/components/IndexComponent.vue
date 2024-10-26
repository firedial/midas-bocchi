<template>
    <div class="container">
        <table class="table table-hover">
            <thead class="thead-light">
                <tr>
                    <th scope="col">項目</th>
                    <th scope="col">ページ</th>
                </tr>
            </thead>
            <tbody>
                <tr v-for="(link, index) in links" :key="index">
                    <td>
                        {{ link.name }}
                    </td>
                    <td>
                        <router-link v-bind:to="{ name: link.url, params: { attributeName: link.attributeName } }">
                            <button class="btn btn-primary">{{ link.value }}</button>
                        </router-link>
                    </td>
                </tr>
            </tbody>
        </table>
    </div>
</template>

<script>
export default {
    data: function () {
        return {
            links: [
                {
                    name: 'ログイン',
                    url: 'login',
                    value: 'login',
                },
                {
                    name: '給料',
                    url: 'salary.create',
                    value: 'salary create',
                },
                {
                    name: 'ボーナス',
                    url: 'bonus.create',
                    value: 'bonus create',
                },
                {
                    name: '月々の支払',
                    url: 'monthly.create',
                    value: 'monthly create',
                },
                {
                    name: '収支表',
                    url: 'balance.list',
                    value: 'balance list',
                },
                {
                    name: '収支追加',
                    url: 'balance.create',
                    value: 'balance add',
                },
                {
                    name: '予算移動リスト',
                    url: 'move.list',
                    value: 'purpose move list',
                    attributeName: 'purposes',
                },
                {
                    name: '予算移動追加',
                    url: 'move.create',
                    value: 'purpose move add',
                    attributeName: 'purposes',
                },
                {
                    name: '場所移動リスト',
                    url: 'move.list',
                    value: 'place move list',
                    attributeName: 'places',
                },
                {
                    name: '場所移動追加',
                    url: 'move.create',
                    value: 'place move add',
                    attributeName: 'places',
                },
                {
                    name: '種別属性要素(element)',
                    url: 'element.list',
                    value: 'kind element add',
                    attributeName: 'kind',
                },
                {
                    name: '予算属性要素(element)',
                    url: 'element.list',
                    value: 'purpose element add',
                    attributeName: 'purpose',
                },
                {
                    name: '場所属性要素(element)',
                    url: 'element.list',
                    value: 'place element add',
                    attributeName: 'place',
                },
                {
                    name: '種別属性カテゴリ(category)',
                    url: 'category.list',
                    value: 'kind category add',
                    attributeName: 'kind',
                },
                {
                    name: '予算属性カテゴリ(category)',
                    url: 'category.list',
                    value: 'purpose category add',
                    attributeName: 'purpose',
                },
                {
                    name: '場所属性カテゴリ(category)',
                    url: 'category.list',
                    value: 'place category add',
                    attributeName: 'place',
                },
                {
                    name: 'ログアウト',
                    url: 'logout',
                    value: 'logout',
                },
            ],
        }
    }
}
</script>