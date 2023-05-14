FROM php:8.0-apache
COPY --from=composer:2.1.14 /usr/bin/composer /usr/bin/composer
RUN apt-get update && apt-get install -y \
    git \
    unzip \
    npm \
    && docker-php-ext-install pdo_mysql
    
RUN sed -i 's!/var/www/html!/var/www/html/public!g' /etc/apache2/sites-available/000-default.conf

RUN  a2enmod rewrite

COPY package*.json ./
RUN npm install

RUN chown -R www-data /var/www/html

