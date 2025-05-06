#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <stack>
#include <queue>
#include <string>
#include <algorithm>
#include <iomanip>

using namespace std;


struct states
{
    unordered_map<char, vector<int>>transitions;
    vector<int> epsilon;
    bool accept = false;
};



bool expressionvalid(const string &input) {
    const string operators = "|+.";
    const string postfixOps = "*+";
    int openparenthesis = 0;

    if (input.empty()) return false;

    for (int i = 0; i < input.size(); ++i) {
        char c = input[i];

        // Check if c is valid (alphanumeric or in operators or postfixOps or parentheses)
        bool isOperator = false;
        for (int j = 0; j < operators.size(); ++j) {
            if (c == operators[j]) {
                isOperator = true;
                break;
            }
        }

        bool isPostfix = false;
        for (int j = 0; j < postfixOps.size(); ++j) {
            if (c == postfixOps[j]) {
                isPostfix = true;
                break;
            }
        }

        if (!isalnum(c) && !isOperator && !isPostfix && c != '(' && c != ')')
            return false;

        if (c == '(') openparenthesis++;
        if (c == ')') {
            openparenthesis--;
            if (openparenthesis < 0) return false;
        }

        if (c == '(' && i + 1 < input.size() && input[i + 1] == ')')
            return false;

        if (i > 0) {
            char prev = input[i - 1];

            bool prevIsOperator = false;
            for (int j = 0; j < operators.size(); ++j) {
                if (prev == operators[j]) {
                    prevIsOperator = true;
                    break;
                }
            }

            if (prevIsOperator && isOperator)
                return false;

            if ((prev == '(' && isOperator) || (prevIsOperator && c == ')'))
                return false;
        }
    }

    return openparenthesis == 0;
}



string Concatenationfunc(const string& input) {
    string result;
    for (int i = 0; i < input.size(); ++i) {
        char c1 = input[i];
        result.push_back(c1);
        if (i + 1 < input.size()) {
            char c2 = input[i + 1];
            bool c1_sym = isalnum(c1), c1_closure = (c1 == '*' || c1 == '+'), c1_close = (c1 == ')');
            bool c2_sym = isalnum(c2), c2_open = (c2 == '(');
            if ((c1_sym || c1_close || c1_closure) && (c2_sym || c2_open)) {
                result.push_back('.');
            }
        }
    }
    return result;
}

int checkoperators(char operators) {
    if (operators == '*' || operators == '+') return 3;
    if (operators == '.') return 2;
    if (operators == '|') return 1;
    return 0;
}

string infixToPostfix(const string& input) {
    string out;
    stack<char> s1;
    for (char c : input) {
        if (isalnum(c)) out += c;
        else if (c == '(')
        {
           s1.push(c);
        }
        else if (c == ')') {
            while (!s1.empty() && s1.top() != '(') {
                out += s1.top(); 
                s1.pop();
            }
            if (!s1.empty()) 
            {
              s1.pop();
            }
        } else {
            while (!s1.empty() && s1.top() != '(' && checkoperators(s1.top()) >= checkoperators(c)) {
                out += s1.top(); s1.pop();
            }
            s1.push(c);
        }
    }
    while (!s1.empty()) {
        out += s1.top();
         s1.pop();
    }
    return out;
}

int createNewState(vector<states>& nfa) {
    nfa.push_back(states());
    return (int)nfa.size() - 1;
}


vector <states> buildNFA(const string &postfix, int &startstate, vector<int> &acceptstates) {
    struct frag
    {
        int start;
        int end;
    };
    stack<frag> st;
    vector<states> nfa;

    for (int i = 0; i < (int)postfix.size(); ++i) {
        char c = postfix[i];
        if (isalnum(c)) {
            int s = createNewState(nfa), f = createNewState(nfa);
            nfa[s].transitions[c].push_back(f);
            nfa[f].accept = true;
            st.push({s, f});
        } 
        else if (c == '.') {
            frag f2 = st.top();
            st.pop();
            frag f1 = st.top();
            st.pop();
            nfa[f1.end].epsilon.push_back(f2.start);
            nfa[f1.end].accept = false;
            st.push({f1.start, f2.end});
        }
        else if (c == '|') {
            frag f2 = st.top();
            st.pop();
            frag f1 = st.top();
            st.pop();
            int S = createNewState(nfa), T = createNewState(nfa);
            nfa[S].epsilon.push_back(f1.start);
            nfa[S].epsilon.push_back(f2.start);
            nfa[f1.end].epsilon.push_back(T);
            nfa[f2.end].epsilon.push_back(T);
            nfa[f1.end].accept = false;
            nfa[f2.end].accept = false;
            nfa[T].accept = true;
            st.push({S, T});
        }
        else if (c == '*') {
            frag f = st.top();
            st.pop();
            nfa[f.end].epsilon.push_back(f.start);
            nfa[f.end].accept = false;
            nfa[f.start].accept = true;
            st.push({f.start, f.start});
        }
        else if (c == '+') {
            frag f = st.top();
            st.pop();
            nfa[f.end].epsilon.push_back(f.start);
            nfa[f.end].accept = false;
            nfa[f.start].accept = false;
            int T = createNewState(nfa);
            nfa[f.end].epsilon.push_back(T);
            nfa[T].accept = true;
            st.push({f.start, T});
        }
    }
    frag e = st.top();
    st.pop();
    startstate = e.start;
    acceptstates.clear();
    for (int i = 0; i < (int)nfa.size(); ++i) {
        if (nfa[i].accept) { 
            acceptstates.push_back(i);
        }
    }
    return nfa;
}

void removeEpsilons(const vector<states>& nfa, int startState,
    const vector<int>& orignalStates,
    vector<unordered_map<char, vector<int>>>& newTrans,
    vector<bool>& newAccept)
{
    int N = nfa.size();
    vector<vector<int>> closure(N);
    for (int i = 0; i < N; ++i) {
        vector<bool> visited(N, false);
        queue<int> q;
        visited[i] = true;
        q.push(i);
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            closure[i].push_back(u);
            for (int j = 0; j < (int)nfa[u].epsilon.size(); ++j) {
                int v = nfa[u].epsilon[j];
                if (!visited[v]) {
                    visited[v] = true;
                    q.push(v);
                }
            }
        }
    }

    newTrans.assign(N, {});
    unordered_set<char> alphabet;
    for (int i = 0; i < N; ++i) {
        for (unordered_map<char, vector<int>>::const_iterator j = nfa[i].transitions.begin(); j != nfa[i].transitions.end(); ++j) {
            alphabet.insert(j->first);
        }
    }

    for (int i = 0; i < N; ++i) {
        for (unordered_set<char>::iterator j = alphabet.begin(); j != alphabet.end(); ++j) {
            char c = *j;
            unordered_set<int> moves;
            for (int k = 0; k < (int)closure[i].size(); ++k) {
                int r = closure[i][k];
                if (nfa[r].transitions.count(c)) {
                    const vector<int>& targets = nfa[r].transitions.at(c);
                    for (int k = 0; k < (int)targets.size(); ++k) {
                        int t = targets[k];
                        for (int s = 0; s < (int)closure[t].size(); ++s) {
                            moves.insert(closure[t][s]);
                        }
                    }
                }
            }
            if (!moves.empty()) {
                vector<int> dest(moves.begin(), moves.end());
                sort(dest.begin(), dest.end());
                newTrans[i][c] = dest;
            }
        }
    }

    newAccept.assign(N, false);
    unordered_set<int> orignalFrag(orignalStates.begin(), orignalStates.end());
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < (int)closure[i].size(); ++j) {
            if (orignalFrag.count(closure[i][j])) {
                newAccept[i] = true;
                break;
            }
        }
    }
}

bool simulatenfa(const vector<unordered_map<char, vector<int>>>& newtrans,const vector<bool>& newaccept, int startstate, const string& input){
	unordered_set<int> currents = {startstate};
	for (int i=0; i<input.length(); i++) {
    char c = input[i];
    unordered_set<int> nextstates;
    
	vector<int> currentvec(currents.begin(), currents.end());
	for (int j=0; j<currentvec.size(); j++) {
        int state = currentvec[j];
            if (newtrans[state].count(c)) {
                const vector<int>& targets = newtrans[state].at(c);
                for (size_t k = 0; k < targets.size(); ++k) {
                    nextstates.insert(targets[k]);
                }
            }
        }
        currents=nextstates;
    }
    vector<int> finalvec(currents.begin(), currents.end());
    for (int i=0; i<finalvec.size(); i++) { 
        if (newaccept[finalvec[i]]) {
            return true;
        }
    }
    return false;
}

void printtransitiontable(const vector<unordered_map<char, vector<int>>>& trans,const vector<bool>& accept, const vector<char>& alphabet){
    cout<<"\nTransition  Table:\n";
    cout<<left<<setw(8)<<"state";
    
     for (int i=0;i<alphabet.size();i++) {  
        cout<<left<<setw(10)<<alphabet[i];
    }
    cout<<"\n"<<string(8 + 10 * alphabet.size(), '-')<<"\n";
    
    for (int i=0;i<trans.size();i++) {  
        cout<<left<<setw(8)<<i;
        
        for (int j=0;j<alphabet.size();j++) { 
            char c=alphabet[j];
            
            if (trans[i].count(c)) {
                stringstream ss;
                ss<<"{";
                for (int k=0;k<trans[i].at(c).size();k++){ 
                    if (k > 0) {
					ss<<",";}
                    ss<<trans[i].at(c)[k];
                }
                ss<<"}";
                cout<<left<<setw(10)<<ss.str();
            }
            else{
            	cout<<left<<setw(10)<<"-";
			}
		}
		if (accept[i]) {
		    cout <<" accept ";}
        cout << "\n";
    }
}
		
main(){
	string input;
    cout<<"Enter the regular expression (use | for union, * for Kleene star, + for plus): ";
    cin >> input;

    if (!expressionvalid(input)){
        cout<<"Invalid regular expression. wrong syntax.\n";
        return 1;
    }
    string modifiedi= Concatenationfunc(input);
    string postfix=infixToPostfix(modifiedi);
    
    int starts;
    vector<int> accepts;
    vector<states> nfa=buildNFA(postfix,starts,accepts);
    
    vector<unordered_map<char, vector<int>>> newtransitions;
    vector<bool> newaccepts;
    removeEpsilons(nfa,starts,accepts,newtransitions,newaccepts);

    unordered_set<char> alphabetSet;
    for (int i=0; i<newtransitions.size(); i++) {
        unordered_map<char, vector<int>> trans=newtransitions[i];
        for (auto it=trans.begin();it!=trans.end();it++) {
            alphabetSet.insert(it->first);
        }
    }
    vector<char> alphabet(alphabetSet.begin(), alphabetSet.end());
    sort(alphabet.begin(), alphabet.end());

    printtransitiontable(newtransitions, newaccepts, alphabet);
    string teststring;
    cout<<"\nEnter a string: ";
    cin>>teststring;
    
    bool accepted=simulatenfa(newtransitions, newaccepts, starts, teststring);

    if(accepted){
        cout<<"The string is accepted\n";
    } else {
        cout<<"The string is not accepted.\n";
    }
    return 0;
}

    
    




    
    
   

