<?php

use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Support\Facades\Schema;

class CreateMPlaceElementTable extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('m_place_element', function (Blueprint $table) {
            $table->id();
            $table->string('name', 20)->unique();
            $table->string('description', 20);
            $table->foreignId('category_id');
            $table->timestamps();

            $table->foreign('category_id')
                ->references('id')
                ->on('m_place_category')
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
        Schema::dropIfExists('m_place_element');
    }
}
