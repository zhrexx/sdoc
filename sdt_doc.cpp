#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <map>
#include <set>
#include <algorithm>
#include <cctype>

using namespace std;

enum TokenType { TOK_EOF, TOK_ID, TOK_STR, TOK_NUM, TOK_LB, TOK_RB, TOK_SEMI, TOK_EQ, TOK_COLON, TOK_COMMA, TOK_AT };

struct Token { TokenType type; string val; };

class Lexer {
    string src;
    size_t pos = 0;
    
    char peek(int off = 0) const { return pos + off < src.size() ? src[pos + off] : 0; }
    char get() { return pos < src.size() ? src[pos++] : 0; }
    
    void skip() {
        while (peek()) {
            if (isspace(peek())) { get(); continue; }
            if (peek() == '#') { while (peek() && peek() != '\n') get(); continue; }
            if (peek() == '/' && peek(1) == '/') { while (peek() && peek() != '\n') get(); continue; }
            if (peek() == '/' && peek(1) == '*') {
                get(); get();
                while (peek() && !(peek() == '*' && peek(1) == '/')) get();
                if (peek()) { get(); get(); }
                continue;
            }
            break;
        }
    }
    
    string str() {
        char q = get();
        string s;
        while (peek() && peek() != q) {
            if (peek() == '\\' && peek(1)) { get(); s += get(); }
            else s += get();
        }
        if (peek() == q) get();
        return s;
    }
    
public:
    Lexer(const string& s) : src(s) {}
    
    Token next() {
        skip();
        if (!peek()) return {TOK_EOF, ""};
        if (peek() == '"' || peek() == '\'') return {TOK_STR, str()};
        if (peek() == '{') { get(); return {TOK_LB, "{"}; }
        if (peek() == '}') { get(); return {TOK_RB, "}"}; }
        if (peek() == ';') { get(); return {TOK_SEMI, ";"}; }
        if (peek() == '=') { get(); return {TOK_EQ, "="}; }
        if (peek() == ':') { get(); return {TOK_COLON, ":"}; }
        if (peek() == ',') { get(); return {TOK_COMMA, ","}; }
        if (peek() == '@') { get(); return {TOK_AT, "@"}; }
        
        if (isdigit(peek()) || (peek() == '-' && isdigit(peek(1)))) {
            string s;
            if (peek() == '-') s += get();
            while (isdigit(peek())) s += get();
            if (peek() == '.') { s += get(); while (isdigit(peek())) s += get(); }
            return {TOK_NUM, s};
        }
        
        string s;
        while (peek() && (isalnum(peek()) || peek() == '_' || peek() == '*' || peek() == '&' || peek() == '<' || peek() == '>')) 
            s += get();
        return {TOK_ID, s};
    }
};

struct Field {
    string type, name, desc, defval;
    vector<string> tags;
    bool required = false;
};

struct Def {
    string kind, name, desc, ret, category, version, author, since, deprecated;
    vector<Field> fields;
    vector<string> links, examples, notes, tags;
    map<string, string> meta;
};

class Parser {
    Lexer lex;
    Token tok;
    
    void eat() { tok = lex.next(); }
    bool match(TokenType t) { if (tok.type == t) { eat(); return true; } return false; }
    bool match(const string& s) { if (tok.val == s) { eat(); return true; } return false; }
    void expect(TokenType t) { if (!match(t)) throw runtime_error("Expected token, got: " + tok.val); }
    
    string type() {
        string t;
        while (tok.type == TOK_ID) {
            t += tok.val;
            eat();
            if (tok.type == TOK_ID && (tok.val[0] == '*' || tok.val[0] == '&')) continue;
            break;
        }
        return t;
    }
    
    vector<string> tagList() {
        vector<string> tags;
        while (tok.type == TOK_ID || tok.type == TOK_STR) {
            tags.push_back(tok.val);
            eat();
            match(TOK_COMMA);
        }
        return tags;
    }
    
    string multilineValue() {
        string val;
        while (tok.type != TOK_SEMI && tok.type != TOK_RB && tok.type != TOK_EOF && tok.type != TOK_AT && tok.val != "links") {
            if (!val.empty() && tok.type != TOK_COMMA) val += " ";
            val += tok.val;
            eat();
        }
        return val;
    }
    
    Field field() {
        Field f;
        
        if (match(TOK_AT)) {
            f.tags = tagList();
            for (const auto& t : f.tags) {
                if (t == "required") f.required = true;
            }
        }
        
        f.type = type();
        if (tok.type == TOK_ID) { f.name = tok.val; eat(); }
        
        if (match(TOK_COLON)) {
            if (tok.type == TOK_STR) { 
                f.desc = tok.val; 
                eat(); 
            } else {
                f.desc = multilineValue();
            }
        }
        
        if (match(TOK_EQ)) {
            if (tok.type == TOK_STR || tok.type == TOK_NUM || tok.type == TOK_ID) {
                f.defval = tok.val; eat();
            }
        }
        
        match(TOK_SEMI);
        return f;
    }
    
public:
    Parser(const string& s) : lex(s) { eat(); }
    
    vector<Def> parse() {
        vector<Def> defs;
        
        while (tok.type != TOK_EOF) {
            Def d;
            
            if (match(TOK_AT)) d.tags = tagList();
            
            if (tok.val == "struct" || tok.val == "union" || tok.val == "fn" || 
                tok.val == "enum" || tok.val == "type" || tok.val == "const" || 
                tok.val == "class" || tok.val == "interface" || tok.val == "trait") {
                d.kind = tok.val; eat();
            } else throw runtime_error("Expected type keyword");
            
            if (tok.type == TOK_ID) { d.name = tok.val; eat(); }
            
            expect(TOK_LB);
            while (tok.type != TOK_RB && tok.type != TOK_EOF) {
                if (match("desc") && match(TOK_COLON)) {
                    if (tok.type == TOK_STR) {
                        d.desc = tok.val;
                        eat();
                    } else {
                        d.desc = multilineValue();
                    }
                    match(TOK_SEMI);
                }
                else if (match("returns") && match(TOK_COLON)) {
                    d.ret = type();
                    match(TOK_SEMI);
                }
                else if (match("links") && match(TOK_COLON)) {
                    while (tok.type == TOK_ID || tok.type == TOK_STR) {
                        d.links.push_back(tok.val); eat();
                        match(TOK_COMMA);
                    }
                    match(TOK_SEMI);
                }
                else if (match("examples") && match(TOK_COLON)) {
                    while (tok.type == TOK_STR) {
                        d.examples.push_back(tok.val); eat();
                        match(TOK_COMMA);
                    }
                    match(TOK_SEMI);
                }
                else if (match("notes") && match(TOK_COLON)) {
                    while (tok.type == TOK_STR) {
                        d.notes.push_back(tok.val); eat();
                        match(TOK_COMMA);
                    }
                    match(TOK_SEMI);
                }
                else if (match("category") && match(TOK_COLON)) {
                    if (tok.type == TOK_ID || tok.type == TOK_STR) {
                        d.category = tok.val; eat();
                    }
                    match(TOK_SEMI);
                }
                else if (match("version") && match(TOK_COLON)) {
                    if (tok.type == TOK_ID || tok.type == TOK_STR || tok.type == TOK_NUM) {
                        d.version = tok.val; eat();
                    }
                    match(TOK_SEMI);
                }
                else if (match("author") && match(TOK_COLON)) {
                    if (tok.type == TOK_ID || tok.type == TOK_STR) {
                        d.author = tok.val; eat();
                    }
                    match(TOK_SEMI);
                }
                else if (match("since") && match(TOK_COLON)) {
                    if (tok.type == TOK_ID || tok.type == TOK_STR || tok.type == TOK_NUM) {
                        d.since = tok.val; eat();
                    }
                    match(TOK_SEMI);
                }
                else if (match("deprecated") && match(TOK_COLON)) {
                    d.deprecated = (tok.type == TOK_STR) ? tok.val : "true";
                    if (tok.type != TOK_SEMI) eat();
                    match(TOK_SEMI);
                }
                else if (match("tags") && match(TOK_COLON)) {
                    while (tok.type == TOK_ID || tok.type == TOK_STR) {
                        d.tags.push_back(tok.val); eat(); match(TOK_COMMA);
                    }
                    match(TOK_SEMI);
                }
                else if (tok.type == TOK_ID) {
                    string key = tok.val;
                    eat();
                    if (match(TOK_COLON)) {
                        if (tok.type == TOK_STR || tok.type == TOK_ID || tok.type == TOK_NUM) {
                            d.meta[key] = tok.val; eat();
                        }
                        match(TOK_SEMI);
                    } else {
                        d.fields.push_back(field());
                    }
                }
                else eat();
            }
            expect(TOK_RB);
            defs.push_back(d);
        }
        
        return defs;
    }
};

string escape(const string& s) {
    string r;
    for (char c : s) {
        switch(c) {
            case '<': r += "&lt;"; break;
            case '>': r += "&gt;"; break;
            case '&': r += "&amp;"; break;
            case '"': r += "&quot;"; break;
            case '\'': r += "&#39;"; break;
            default: r += c;
        }
    }
    return r;
}

string linkify(const string& type, const map<string, string>& nameMap) {
    string result, current;
    
    for (char c : type) {
        if (isalnum(c) || c == '_') {
            current += c;
        } else {
            if (!current.empty()) {
                result += nameMap.count(current) ? 
                    "<a href=\"" + nameMap.at(current) + "\" class=\"type-link\">" + escape(current) + "</a>" :
                    escape(current);
                current.clear();
            }
            result += escape(string(1, c));
        }
    }
    
    if (!current.empty()) {
        result += nameMap.count(current) ? 
            "<a href=\"" + nameMap.at(current) + "\" class=\"type-link\">" + escape(current) + "</a>" :
            escape(current);
    }
    
    return result;
}

string getStyle() {
    return R"(
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #0d1117; color: #c9d1d9; line-height: 1.6; }
.container { max-width: 1400px; margin: 0 auto; padding: 20px; }
.header { padding: 30px 0; border-bottom: 1px solid #30363d; margin-bottom: 30px; }
.header h1 { font-size: 32px; font-weight: 600; color: #f0f6fc; margin-bottom: 8px; }
.header .subtitle { color: #8b949e; font-size: 16px; }
.header .stats { display: flex; gap: 20px; margin-top: 15px; font-size: 14px; color: #8b949e; }
.stat-item { display: flex; align-items: center; gap: 6px; }
.nav { display: flex; gap: 20px; margin: 20px 0; padding: 15px 0; border-bottom: 1px solid #30363d; flex-wrap: wrap; }
.nav a { color: #58a6ff; text-decoration: none; font-size: 14px; padding: 8px 16px; border-radius: 6px; transition: all 0.2s; }
.nav a:hover { background: #161b22; }
.nav a.active { background: #0d419d; color: #fff; }
.search-container { display: flex; gap: 10px; margin-bottom: 20px; flex-wrap: wrap; }
.search-box { flex: 1; min-width: 300px; padding: 10px 16px; background: #0d1117; border: 1px solid #30363d; border-radius: 6px; color: #c9d1d9; font-size: 14px; }
.search-box:focus { outline: none; border-color: #58a6ff; box-shadow: 0 0 0 3px rgba(88, 166, 255, 0.1); }
.view-toggle { display: flex; gap: 5px; }
.view-btn { padding: 8px 12px; background: #21262d; border: 1px solid #30363d; border-radius: 6px; color: #8b949e; cursor: pointer; transition: all 0.2s; }
.view-btn:hover { border-color: #58a6ff; color: #58a6ff; }
.view-btn.active { background: #0d419d; color: #fff; border-color: #0d419d; }
.filters { display: flex; gap: 10px; flex-wrap: wrap; margin-bottom: 20px; }
.filter-btn { padding: 6px 12px; background: #21262d; border: 1px solid #30363d; border-radius: 6px; color: #8b949e; cursor: pointer; font-size: 13px; transition: all 0.2s; }
.filter-btn:hover { border-color: #58a6ff; color: #58a6ff; }
.filter-btn.active { background: #0d419d; color: #fff; border-color: #0d419d; }
.grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(350px, 1fr)); gap: 20px; }
.list-view { display: flex; flex-direction: column; gap: 15px; }
.card { background: #161b22; border: 1px solid #30363d; border-radius: 6px; padding: 20px; transition: all 0.2s; }
.card:hover { border-color: #58a6ff; transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3); }
.card-header { display: flex; align-items: center; gap: 10px; margin-bottom: 12px; }
.badge { display: inline-block; padding: 4px 10px; border-radius: 4px; font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px; }
.badge-fn { background: #1f6feb; color: #fff; }
.badge-struct { background: #2ea043; color: #fff; }
.badge-enum { background: #da3633; color: #fff; }
.badge-union { background: #8957e5; color: #fff; }
.badge-type { background: #d29922; color: #000; }
.badge-const { background: #6e7681; color: #fff; }
.badge-class { background: #0969da; color: #fff; }
.badge-interface { background: #1a7f37; color: #fff; }
.badge-trait { background: #bf4b8a; color: #fff; }
.card-title { font-size: 18px; font-weight: 600; color: #f0f6fc; }
.card-title a { color: #58a6ff; text-decoration: none; }
.card-title a:hover { text-decoration: underline; }
.card-desc { color: #8b949e; font-size: 14px; margin: 10px 0; line-height: 1.5; }
.tags { display: flex; gap: 6px; flex-wrap: wrap; margin-top: 12px; }
.tag { background: #21262d; color: #58a6ff; padding: 3px 8px; border-radius: 3px; font-size: 11px; border: 1px solid #30363d; }
.meta-info { display: flex; gap: 15px; margin-top: 12px; font-size: 12px; color: #8b949e; flex-wrap: wrap; }
.deprecated-badge { background: #da3633; color: #fff; padding: 4px 8px; border-radius: 4px; font-size: 11px; font-weight: 600; }
.required-badge { background: #f85149; color: #fff; padding: 2px 6px; border-radius: 3px; font-size: 10px; margin-left: 5px; }
.detail-page { max-width: 1200px; margin: 0 auto; }
.detail-header { background: #161b22; border: 1px solid #30363d; border-radius: 6px; padding: 30px; margin-bottom: 20px; }
.detail-title { font-size: 36px; font-weight: 600; color: #f0f6fc; margin-bottom: 15px; display: flex; align-items: center; gap: 15px; flex-wrap: wrap; }
.detail-desc { font-size: 16px; color: #c9d1d9; margin: 15px 0; line-height: 1.7; }
.section { background: #161b22; border: 1px solid #30363d; border-radius: 6px; padding: 25px; margin-bottom: 20px; }
.section-title { font-size: 20px; font-weight: 600; color: #f0f6fc; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #30363d; }
.field-table { width: 100%; border-collapse: collapse; }
.field-table th { text-align: left; padding: 12px; background: #0d1117; color: #8b949e; font-weight: 600; font-size: 13px; text-transform: uppercase; letter-spacing: 0.5px; border-bottom: 1px solid #30363d; }
.field-table td { padding: 12px; border-bottom: 1px solid #21262d; vertical-align: top; }
.field-table tr:hover { background: #0d1117; }
.type { color: #ff7b72; font-family: 'SF Mono', Monaco, Consolas, monospace; font-size: 13px; }
.type-link { color: #79c0ff; text-decoration: none; }
.type-link:hover { text-decoration: underline; }
.name { color: #f0f6fc; font-weight: 500; font-family: 'SF Mono', Monaco, Consolas, monospace; font-size: 13px; }
.default { color: #7ee787; font-family: 'SF Mono', Monaco, Consolas, monospace; font-size: 13px; }
.returns-box { background: #0d1117; padding: 15px; border-radius: 6px; border: 1px solid #30363d; }
.links-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 10px; }
.link-item { padding: 10px; background: #0d1117; border: 1px solid #30363d; border-radius: 4px; }
.link-item a { color: #58a6ff; text-decoration: none; font-size: 14px; }
.link-item a:hover { text-decoration: underline; }
.example-block { background: #0d1117; border: 1px solid #30363d; border-radius: 6px; padding: 15px; margin: 10px 0; }
.example-block pre { color: #c9d1d9; overflow-x: auto; white-space: pre-wrap; word-wrap: break-word; }
.note-block { background: #1c2128; border-left: 3px solid #58a6ff; padding: 12px 15px; margin: 10px 0; border-radius: 4px; }
.category-badge { background: #21262d; color: #58a6ff; padding: 6px 12px; border-radius: 4px; font-size: 12px; border: 1px solid #30363d; }
.sidebar { position: sticky; top: 20px; background: #161b22; border: 1px solid #30363d; border-radius: 6px; padding: 20px; max-height: calc(100vh - 40px); overflow-y: auto; }
.sidebar-title { font-size: 14px; font-weight: 600; color: #8b949e; margin-bottom: 10px; text-transform: uppercase; letter-spacing: 0.5px; }
.sidebar-list { list-style: none; }
.sidebar-list li { margin: 6px 0; }
.sidebar-list a { color: #8b949e; text-decoration: none; font-size: 14px; display: block; padding: 4px 8px; border-radius: 4px; transition: all 0.2s; }
.sidebar-list a:hover { background: #0d1117; color: #58a6ff; }
.sidebar-list a.current { background: #0d419d; color: #fff; }
.two-column { display: grid; grid-template-columns: 250px 1fr; gap: 30px; }
.no-results { text-align: center; padding: 60px 20px; color: #8b949e; }
.no-results h3 { font-size: 20px; margin-bottom: 10px; }
@media (max-width: 900px) {
    .two-column { grid-template-columns: 1fr; }
    .sidebar { position: static; max-height: none; }
    .grid { grid-template-columns: 1fr; }
}
)";
}

void generateIndex(const vector<Def>& defs, const string& outdir) {
    ofstream f(outdir + "/index.html");
    
    set<string> allTags, allCategories;
    map<string, int> kindCount;
    for (const auto& d : defs) {
        for (const auto& t : d.tags) allTags.insert(t);
        if (!d.category.empty()) allCategories.insert(d.category);
        kindCount[d.kind]++;
    }
    
    f << "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n";
    f << "<title>API Documentation</title>\n<style>\n" << getStyle() << "</style>\n</head>\n<body>\n";
    f << "<div class=\"container\">\n";
    f << "<div class=\"header\">\n<h1>API Documentation</h1>\n<div class=\"subtitle\">Complete reference for all types and functions</div>\n";
    f << "<div class=\"stats\">\n";
    f << "<span class=\"stat-item\">📦 " << defs.size() << " Total Items</span>\n";
    for (const auto& kc : kindCount) {
        f << "<span class=\"stat-item\">" << kc.second << " " << kc.first << "</span>\n";
    }
    f << "</div>\n</div>\n";
    
    f << "<div class=\"search-container\">\n";
    f << "<input type=\"text\" class=\"search-box\" id=\"search\" placeholder=\"Search by name, description, or tags...\" onkeyup=\"filterItems()\">\n";
    f << "<div class=\"view-toggle\">\n";
    f << "<button class=\"view-btn active\" onclick=\"setView('grid')\">Grid</button>\n";
    f << "<button class=\"view-btn\" onclick=\"setView('list')\">List</button>\n";
    f << "</div>\n</div>\n";
    
    f << "<div class=\"filters\">\n";
    f << "<button class=\"filter-btn active\" onclick=\"filterByType('all')\">All</button>\n";
    for (const auto& kc : kindCount) {
        f << "<button class=\"filter-btn\" onclick=\"filterByType('" << kc.first << "')\">" << kc.first << "</button>\n";
    }
    if (!allCategories.empty()) {
        f << "<span style=\"color: #30363d; margin: 0 5px;\">|</span>\n";
        for (const auto& cat : allCategories) {
            f << "<button class=\"filter-btn\" onclick=\"filterByCategory('" << escape(cat) << "')\">" << escape(cat) << "</button>\n";
        }
    }
    f << "</div>\n";
    
    f << "<div class=\"grid\" id=\"items\">\n";
    
    for (const auto& d : defs) {
        string searchText = d.name + " " + d.desc;
        for (const auto& t : d.tags) searchText += " " + t;
        
        f << "<div class=\"card\" data-type=\"" << d.kind << "\" data-category=\"" << escape(d.category) << "\" data-name=\"" << escape(d.name) << "\"";
        f << " data-search=\"" << escape(searchText) << "\">\n";
        
        f << "<div class=\"card-header\">\n";
        f << "<span class=\"badge badge-" << d.kind << "\">" << d.kind << "</span>\n";
        if (!d.deprecated.empty()) f << "<span class=\"deprecated-badge\">deprecated</span>\n";
        f << "</div>\n";
        
        f << "<div class=\"card-title\"><a href=\"" << d.name << ".html\">" << escape(d.name) << "</a></div>\n";
        if (!d.desc.empty()) f << "<div class=\"card-desc\">" << escape(d.desc) << "</div>\n";
        
        if (!d.category.empty() || !d.version.empty() || !d.since.empty()) {
            f << "<div class=\"meta-info\">\n";
            if (!d.category.empty()) f << "<span>📁 " << escape(d.category) << "</span>\n";
            if (!d.version.empty()) f << "<span>v" << escape(d.version) << "</span>\n";
            if (!d.since.empty()) f << "<span>Since " << escape(d.since) << "</span>\n";
            f << "</div>\n";
        }
        
        if (!d.tags.empty()) {
            f << "<div class=\"tags\">\n";
            for (const auto& t : d.tags) f << "<span class=\"tag\">" << escape(t) << "</span>\n";
            f << "</div>\n";
        }
        
        f << "</div>\n";
    }
    
    f << "</div>\n";
    f << "<div class=\"no-results\" id=\"no-results\" style=\"display: none;\">\n";
    f << "<h3>No results found</h3>\n<p>Try adjusting your search or filters</p>\n</div>\n";
    f << "</div>\n";
    
    f << R"(<script>
let currentType = 'all', currentCategory = '', currentView = 'grid';

function setView(view) {
    currentView = view;
    const items = document.getElementById('items');
    items.className = view === 'grid' ? 'grid' : 'list-view';
    document.querySelectorAll('.view-btn').forEach(btn => {
        btn.classList.toggle('active', btn.textContent.toLowerCase() === view);
    });
}

function filterByType(type) {
    currentType = type;
    currentCategory = '';
    filterItems();
    document.querySelectorAll('.filter-btn').forEach(btn => {
        btn.classList.remove('active');
    });
    event.target.classList.add('active');
}

function filterByCategory(cat) {
    currentCategory = cat;
    currentType = 'all';
    filterItems();
    document.querySelectorAll('.filter-btn').forEach(btn => {
        btn.classList.remove('active');
    });
    event.target.classList.add('active');
}

function filterItems() {
    const search = document.getElementById('search').value.toLowerCase();
    const items = document.querySelectorAll('.card');
    let visible = 0;
    
    items.forEach(item => {
        const type = item.getAttribute('data-type');
        const category = item.getAttribute('data-category');
        const searchText = item.getAttribute('data-search').toLowerCase();
        
        const matchType = currentType === 'all' || type === currentType;
        const matchCategory = !currentCategory || category === currentCategory;
        const matchSearch = !search || searchText.includes(search);
        
        if (matchType && matchCategory && matchSearch) {
            item.style.display = 'block';
            visible++;
        } else {
            item.style.display = 'none';
        }
    });
    
    document.getElementById('no-results').style.display = visible === 0 ? 'block' : 'none';
}
</script>
)";
    
    f << "</body>\n</html>\n";
}

void generatePage(const Def& def, const map<string, string>& nameMap, const string& outdir, const vector<Def>& allDefs) {
    ofstream f(outdir + "/" + def.name + ".html");
    
    map<string, vector<string>> categories;
    for (const auto& d : allDefs) {
        string cat = d.category.empty() ? "General" : d.category;
        categories[cat].push_back(d.name);
    }
    
    f << "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n";
    f << "<title>" << escape(def.name) << " - SDOC Documentation</title>\n<style>\n" << getStyle() << "</style>\n</head>\n<body>\n";
    f << "<div class=\"container\">\n<div class=\"two-column\">\n";
    
    f << "<div class=\"sidebar\">\n";
    f << "<div class=\"sidebar-title\">Navigation</div>\n";
    f << "<ul class=\"sidebar-list\">\n<li><a href=\"index.html\">← Back to Index</a></li>\n</ul>\n";
    
    for (const auto& cat : categories) {
        f << "<div class=\"sidebar-title\" style=\"margin-top: 20px;\">" << escape(cat.first) << "</div>\n";
        f << "<ul class=\"sidebar-list\">\n";
        for (const auto& name : cat.second) {
            string cls = (name == def.name) ? " class=\"current\"" : "";
            f << "<li><a href=\"" << name << ".html\"" << cls << ">" << escape(name) << "</a></li>\n";
        }
        f << "</ul>\n";
    }
    
    f << "</div>\n";
    
    f << "<div class=\"detail-page\">\n";
    f << "<div class=\"detail-header\">\n";
    f << "<div class=\"detail-title\">\n" << escape(def.name) << " <span class=\"badge badge-" << def.kind << "\">" << def.kind << "</span>\n";
    if (!def.deprecated.empty()) f << "<span class=\"deprecated-badge\">deprecated</span>\n";
    f << "</div>\n";
    
    if (!def.desc.empty()) f << "<div class=\"detail-desc\">" << escape(def.desc) << "</div>\n";
    
    if (!def.deprecated.empty() && def.deprecated != "true") {
        f << "<div class=\"note-block\" style=\"border-left-color: #da3633; background: #2d1417;\">\n";
        f << "<strong>⚠️ Deprecated:</strong> " << escape(def.deprecated) << "\n</div>\n";
    }
    
    if (!def.category.empty() || !def.version.empty() || !def.author.empty() || !def.since.empty()) {
        f << "<div class=\"meta-info\" style=\"margin-top: 15px;\">\n";
        if (!def.category.empty()) f << "<span class=\"category-badge\">" << escape(def.category) << "</span>\n";
        if (!def.version.empty()) f << "<span>Version: " << escape(def.version) << "</span>\n";
        if (!def.since.empty()) f << "<span>Since: " << escape(def.since) << "</span>\n";
        if (!def.author.empty()) f << "<span>Author: " << escape(def.author) << "</span>\n";
        f << "</div>\n";
    }
    
    if (!def.tags.empty()) {
        f << "<div class=\"tags\" style=\"margin-top: 15px;\">\n";
        for (const auto& t : def.tags) f << "<span class=\"tag\">" << escape(t) << "</span>\n";
        f << "</div>\n";
    }
    
    f << "</div>\n";
    
    if (!def.ret.empty()) {
        f << "<div class=\"section\">\n<div class=\"section-title\">Returns</div>\n";
        f << "<div class=\"returns-box\"><span class=\"type\">" << linkify(def.ret, nameMap) << "</span></div>\n</div>\n";
    }
    
    if (!def.fields.empty()) {
        f << "<div class=\"section\">\n<div class=\"section-title\">Fields</div>\n";
        f << "<table class=\"field-table\">\n<thead>\n<tr><th>Name</th><th>Type</th><th>Description</th><th>Default</th></tr>\n</thead>\n<tbody>\n";
        for (const auto& field : def.fields) {
            f << "<tr>\n";
            f << "<td><span class=\"name\">" << escape(field.name) << "</span>";
            if (field.required) f << "<span class=\"required-badge\">REQUIRED</span>";
            if (!field.tags.empty()) {
                f << "<div class=\"tags\" style=\"margin-top: 5px;\">";
                for (const auto& t : field.tags) f << "<span class=\"tag\">" << escape(t) << "</span> ";
                f << "</div>";
            }
            f << "</td>\n";
            f << "<td><span class=\"type\">" << linkify(field.type, nameMap) << "</span></td>\n";
            f << "<td>" << escape(field.desc) << "</td>\n";
            f << "<td>" << (field.defval.empty() ? "-" : "<span class=\"default\">" + escape(field.defval) + "</span>") << "</td>\n";
            f << "</tr>\n";
        }
        f << "</tbody>\n</table>\n</div>\n";
    }
    
    if (!def.examples.empty()) {
        f << "<div class=\"section\">\n<div class=\"section-title\">Examples</div>\n";
        for (const auto& ex : def.examples) {
            f << "<div class=\"example-block\">\n<pre>" << escape(ex) << "</pre>\n</div>\n";
        }
        f << "</div>\n";
    }
    
    if (!def.notes.empty()) {
        f << "<div class=\"section\">\n<div class=\"section-title\">Notes</div>\n";
        for (const auto& note : def.notes) {
            f << "<div class=\"note-block\">💡 " << escape(note) << "</div>\n";
        }
        f << "</div>\n";
    }
    
    if (!def.links.empty()) {
        f << "<div class=\"section\">\n<div class=\"section-title\">Related Items</div>\n";
        f << "<div class=\"links-grid\">\n";
        for (const auto& link : def.links) {
            f << "<div class=\"link-item\">";
            if (nameMap.count(link)) {
                f << "<a href=\"" << nameMap.at(link) << "\">" << escape(link) << "</a>";
            } else {
                f << escape(link);
            }
            f << "</div>\n";
        }
        f << "</div>\n</div>\n";
    }
    
    if (!def.meta.empty()) {
        f << "<div class=\"section\">\n<div class=\"section-title\">Additional Metadata</div>\n";
        f << "<table class=\"field-table\">\n<tbody>\n";
        for (const auto& m : def.meta) {
            f << "<tr><td style=\"width: 200px;\"><strong>" << escape(m.first) << "</strong></td><td>" << escape(m.second) << "</td></tr>\n";
        }
        f << "</tbody>\n</table>\n</div>\n";
    }
    
    f << "</div>\n</div>\n</div>\n</body>\n</html>\n";
}

int main(int argc, char **argv) {
    if (argc < 3) {
        cerr << "SDOC - Simple Documentation Generator\n";
        cerr << "Usage: " << argv[0] << " <input_file> <output_dir>\n";
        cerr << "Generates comprehensive HTML documentation from SDOC definition files.\n";
        return 1;
    }
    
    ifstream inFile(argv[1]);
    if (!inFile) {
        cerr << "Error: Cannot open input file '" << argv[1] << "'\n";
        return 1;
    }
    
    stringstream buffer;
    buffer << inFile.rdbuf();
    string input = buffer.str();
    inFile.close();
    
    string outdir = argv[2];
    
    try {
        Parser p(input);
        auto defs = p.parse();
        
        if (defs.empty()) {
            cerr << "Warning: No definitions found in input file\n";
            return 1;
        }
        
        map<string, string> nameMap;
        for (const auto& d : defs) {
            nameMap[d.name] = d.name + ".html";
        }
        
        generateIndex(defs, outdir);
        
        for (const auto& d : defs) {
            generatePage(d, nameMap, outdir, defs);
        }
        
        cout << "✓ SDOC documentation generated successfully!\n";
        cout << "  Output directory: " << outdir << "/\n";
        cout << "  Total pages: " << defs.size() + 1 << " (" << defs.size() << " detail pages + 1 index)\n";
        cout << "  Open " << outdir << "/index.html in your browser\n";
        
    } catch (const exception& e) {
        cerr << "Error: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}
