package neko.splash;

import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.ImageView;
import android.widget.TextView;

import clojure.lang.Symbol;
import clojure.lang.Var;
import clojure.lang.RT;

import org.clojure_android.neko.R;

public class SplashActivity extends Activity {

    private static boolean firstLaunch = true;

    @Override
    public void onCreate(Bundle bundle) {
        super.onCreate(bundle);

        if (firstLaunch) {
            firstLaunch = false;
            setupSplash();
            loadClojure();
        } else {
            proceed();
        }
    }

    public void setupSplash() {
        // TODO: This is despicably terrible. Rewrite this ASAP.
        setContentView(R.layout.splashscreen);
        PackageManager pm = getPackageManager();
        CharSequence title = "<Unknown>";
        try {
            title = pm.getApplicationLabel(pm.getApplicationInfo(getApplicationInfo().packageName, 0));
        } catch (Exception e) { }
        TextView appNameView = (TextView)findViewById(R.id.splash_app_name + 65536);
        if (appNameView != null) {
            appNameView.setText(title);
        }

        Animation rotation = AnimationUtils.loadAnimation(this, R.anim.splash_rotation);
        ImageView circleView = (ImageView)findViewById(R.id.splash_circles + 65536);
        if (circleView != null) {
            circleView.startAnimation(rotation);
        }
    }

    public void proceed() {
        startActivity(new Intent(getPackageName() + ".MAIN"));
        finish();
    }

    public void loadClojure() {
        new Thread(new Runnable(){
                @Override
                public void run() {
                    Symbol CLOJURE_MAIN = Symbol.intern("neko.application");
                    Var REQUIRE = RT.var("clojure.core", "require");
                    REQUIRE.invoke(CLOJURE_MAIN);

                    Var INIT = RT.var("neko.application", "init-application");
                    INIT.invoke(SplashActivity.this.getApplication());

                    proceed();
                }
            }).start();
    }
}
