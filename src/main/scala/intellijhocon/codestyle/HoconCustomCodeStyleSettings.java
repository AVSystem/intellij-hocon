package intellijhocon.codestyle;

import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.codeStyle.CustomCodeStyleSettings;

public class HoconCustomCodeStyleSettings extends CustomCodeStyleSettings {
    public HoconCustomCodeStyleSettings(CodeStyleSettings container) {
        super("HoconCodeStyleSettings", container);
    }

    // SPACING
    // Around operators
    public boolean SPACE_BEFORE_COLON = false;
    public boolean SPACE_AFTER_COLON = true;
    public boolean SPACE_BEFORE_ASSIGNMENT = true;
    public boolean SPACE_AFTER_ASSIGNMENT = true;
    // Before left brace
    public boolean SPACE_BEFORE_LBRACE_AFTER_PATH = true;
    // Within
    public boolean SPACES_WITHIN_REFERENCE_BRACES = false;
    // Other
    public boolean SPACE_AFTER_QMARK = false;

    //WRAPPING AND BRACES
    //Keep when reformatting
    public boolean KEEP_SIMPLE_LISTS_IN_ONE_LINE = false;
    public boolean HASH_COMMENTS_AT_FIRST_COLUMN = false;
    public boolean DOUBLE_SLASH_COMMENTS_AT_FIRST_COLUMN = false;

    public int OBJECTS_WRAP = CommonCodeStyleSettings.DO_NOT_WRAP;
    public boolean OBJECTS_ALIGN_WHEN_MULTILINE = false;
    public boolean OBJECTS_LBRACE_ON_NEXT_LINE = true;
    public boolean OBJECTS_RBRACE_ON_NEXT_LINE = true;

    public int LISTS_WRAP = CommonCodeStyleSettings.DO_NOT_WRAP;
    public boolean LISTS_ALIGN_WHEN_MULTILINE = false;
    public boolean LISTS_LBRACKET_ON_NEXT_LINE = false;
    public boolean LISTS_RBRACKET_ON_NEXT_LINE = false;

    public int OBJECT_FIELDS_WITH_COLON_WRAP = CommonCodeStyleSettings.DO_NOT_WRAP;
    public boolean OBJECT_FIELDS_COLON_ON_NEXT_LINE = false;

    public int OBJECT_FIELDS_WITH_ASSIGNMENT_WRAP = CommonCodeStyleSettings.DO_NOT_WRAP;
    public boolean OBJECT_FIELDS_ASSIGNMENT_ON_NEXT_LINE = false;
}
