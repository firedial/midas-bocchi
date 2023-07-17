import RPi.GPIO as GPIO
from time import sleep
import subprocess

'''
下記コマンドでバックグランドで起動しておく
$ nohup python button.py &
'''

GPIO.setmode(GPIO.BCM)

# 緑に接続
GPIO.setup(25, GPIO.OUT)
# 赤に接続
GPIO.setup(24, GPIO.OUT)
# ボタンに接続
GPIO.setup(27, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)
# スイッチに接続
GPIO.setup(17, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)

# rikka コンテナの名前
DOCKER_CONTEINER_RIKKA_NAME = 'midas-bocchi_rikka_1'


def deploy():
    # ボタン押された時
    # 一旦両方つける
    GPIO.output(25, GPIO.HIGH)
    GPIO.output(24, GPIO.HIGH)

    # イメージのプル
    cp = subprocess.run(['docker', 'pull', 'firedial/midas-bocchi-haruhi'])

    if int(cp.returncode) != 0:
        # 失敗した時は赤をつける
        GPIO.output(25, GPIO.LOW)
        GPIO.output(24, GPIO.HIGH)

        # 1秒間点灯させて消す
        sleep(1)
        GPIO.output(25, GPIO.LOW)
        GPIO.output(24, GPIO.LOW)
        return

    # コンテナに反映
    cp = subprocess.run(['docker-compose', '-f', 'docker-compose-product.yml', 'up', '-d'])

    if int(cp.returncode) == 0:
        # 成功した時は緑をつける
        GPIO.output(25, GPIO.HIGH)
        GPIO.output(24, GPIO.LOW)
        subprocess.run(['docker', 'exec', DOCKER_CONTEINER_RIKKA_NAME, '/home/root/discord/notify.sh', 'deploy success!'])
    else:
        # 失敗した時は赤をつける
        GPIO.output(25, GPIO.LOW)
        GPIO.output(24, GPIO.HIGH)
        subprocess.run(['docker', 'exec', DOCKER_CONTEINER_RIKKA_NAME, '/home/root/discord/notify.sh', 'deploy fail!'])

    # 1秒間点灯させて消す
    sleep(1)
    GPIO.output(25, GPIO.LOW)
    GPIO.output(24, GPIO.LOW)


def backup():
    # ボタン押された時
    # 一旦両方つける
    GPIO.output(25, GPIO.HIGH)
    GPIO.output(24, GPIO.HIGH)

    # バックアップコマンド実行
    cp = subprocess.run(['docker', 'exec', DOCKER_CONTEINER_RIKKA_NAME, '/home/root/backup/main.sh'])

    if int(cp.returncode) == 0:
        # 成功した時は緑をつける
        GPIO.output(25, GPIO.HIGH)
        GPIO.output(24, GPIO.LOW)
    else:
        # 失敗した時は赤をつける
        GPIO.output(25, GPIO.LOW)
        GPIO.output(24, GPIO.HIGH)

    # 1秒間点灯させて消す
    sleep(1)
    GPIO.output(25, GPIO.LOW)
    GPIO.output(24, GPIO.LOW)

flashingCount = 0
try:
    while True:
        if GPIO.input(17) == GPIO.HIGH:
            # デプロイモード

            # 点滅させる
            if flashingCount < 50:
                GPIO.output(25, GPIO.HIGH)
                GPIO.output(24, GPIO.LOW)
            else:
                GPIO.output(25, GPIO.LOW)
                GPIO.output(24, GPIO.HIGH)

            flashingCount += 1
            if flashingCount >= 100:
                flashingCount = 0

            # ボタン押されたらデプロイする
            if GPIO.input(27) == GPIO.HIGH:
                deploy()


        else:
            # バックアップモード
            if GPIO.input(27) == GPIO.HIGH:
                backup()

        sleep(0.01)

except KeyboardInterrupt:
    pass

GPIO.cleanup()
