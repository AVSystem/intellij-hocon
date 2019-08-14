/*
 * Copyright 2000-2008 JetBrains s.r.o.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jetbrains.plugins.scala.util;

import com.intellij.openapi.diagnostic.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

/**
 * Created by IntelliJ IDEA.
 * User: Ilya.Sergey
 */
public class TestUtils {
    private static final Logger LOG = Logger.getInstance("org.jetbrains.plugins.scala.util.TestUtils");

    private static String TEST_DATA_PATH = null;

    @NotNull
    public static String getTestDataPath() {
        if (TEST_DATA_PATH == null) {
            ClassLoader loader = TestUtils.class.getClassLoader();
            URL resource = loader.getResource("testdata");
            try {
                File f1 = new File("community/scala/scala-impl", "testdata");
                if (resource != null) {
                    TEST_DATA_PATH = new File(resource.toURI()).getPath().replace(File.separatorChar, '/');
                } else if (f1.exists()) {
                    TEST_DATA_PATH = f1.getAbsolutePath();
                } else {
                    File f2 = findTestDataDir(new File("scala/scala-impl").getCanonicalFile());
                    TEST_DATA_PATH = f2.getAbsolutePath();
                }
            } catch (URISyntaxException | IOException e) {
                LOG.error(e);
                throw new RuntimeException(e);
                // just rethrowing here because that's a clearer way to make tests fail than some NPE somewhere else
            }
        }

        return TEST_DATA_PATH;
    }

    /**
     * Go upwards to find testdata, because when running test from IDEA, the launching dir might be some subdirectory.
     */
    @NotNull
    private static File findTestDataDir(File here) throws IOException {
        File testdata = new File(here, "testdata").getCanonicalFile();
        if (testdata.exists()) return testdata;
        else {
            File parent = here.getParentFile();
            if (parent == null) throw new RuntimeException("no testdata directory found");
            else return findTestDataDir(parent);
        }
    }
}
