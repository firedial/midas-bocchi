import RPi.GPIO as GPIO
from time import sleep
import subprocess

'''
下記コマンドでバックグランドで起動しておく
$ nohup python backup.py &
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

# バックアップコマンド
command = ['docker', 'exec', 'midas-bocchi_rikka_1', '/home/root/backup/main.sh']

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

            if GPIO.input(27) == GPIO.HIGH:
                # ボタン押された時
                # 一旦両方つける
                GPIO.output(25, GPIO.HIGH)
                GPIO.output(24, GPIO.HIGH)

                # イメージのプル
                dockerPullCommand = ['docker', 'pull', 'firedial/midas-bocchi-haruhi']
                cp = subprocess.run(dockerPullCommand)

                # コンテナに反映
                dockerCommand = ['docker-compose', '-f', 'docker-compose-product.yml', 'up', '-d']
                cp = subprocess.run(dockerCommand)

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
        elif GPIO.input(27) == GPIO.HIGH:
            # ボタン押された時
            # 一旦両方つける
            GPIO.output(25, GPIO.HIGH)
            GPIO.output(24, GPIO.HIGH)

            # バックアップコマンド実行
            cp = subprocess.run(command)

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

        sleep(0.01)

except KeyboardInterrupt:
    pass

GPIO.cleanup()
