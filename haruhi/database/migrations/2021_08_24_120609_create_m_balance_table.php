<?php

use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Support\Facades\Schema;

class CreateMBalanceTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('m_balance', function (Blueprint $table) {
            $table->id();
            $table->integer('amount');
            $table->string('item', 50);
            $table->foreignId('kind_element_id');
            $table->foreignId('purpose_element_id');
            $table->foreignId('place_element_id');
            $table->date('date');
            $table->timestamps();

            $table->foreign('kind_element_id')
                ->references('id')
                ->on('m_kind_element')
                ->onDelete('restrict');

            $table->foreign('purpose_element_id')
                ->references('id')
                ->on('m_purpose_element')
                ->onDelete('restrict');

            $table->foreign('place_element_id')
                ->references('id')
                ->on('m_place_element')
                ->onDelete('restrict');
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::dropIfExists('m_balance');
    }
}
