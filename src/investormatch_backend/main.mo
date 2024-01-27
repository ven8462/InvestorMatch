import Nat "mo:base/Nat";
import Bool "mo:base/Bool";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Hash "mo:base/Hash";
import Trie "mo:base/Trie";
import Error "mo:base/Error";
import Iter "mo:base/Iter";



actor {
    // define a record type for the Project
    type Project = {
        goal: Nat;
        owner: Principal;
        contributionDestination: Text;
        ended: Bool;
        totalRaised: Nat;
        
    };

// define a key type for the trie
// a trie is a map from keys to values
// the key type is a type that can be hashed and compared for equality
    func key(t: Text) : Trie.Key<Text> { { hash = Text.hash t; key = t } };

// define a trie to store the projects
    var projects : Trie.Trie<Text, Project> = Trie.empty();

// define the public functions of the actor
// create a project
    public shared(msg) func createProject(name: Text, goal: Nat, destination: Text) : async Project {
    // get the principal of the caller
    let owner = msg.caller;
    
    // check if the user is authenticated
    if (isAuthenticated(owner)) {
        // create the project
        let project = { goal = goal; owner = owner; contributionDestination = destination; ended = false; totalRaised = 0 };
        // put the project in the trie
        let (newProjects, _) = Trie.put(projects, key(name), Text.equal, project);
        projects := newProjects;
        // return the project
        return project;
    } else {
        throw Error.reject("User is not authenticated");
    }
};

// get the details of a project
    public query func getProjectDetails(name: Text) : async Project {
        let maybeProject = Trie.get(projects, key(name), Text.equal);
        switch (maybeProject) {
            case (?project) project;
            case null { throw Error.reject("Project not found") };
        };
    };

// contribute to a project
    func contributeToProject(name: Text, amount: Nat) : async () {
    let maybeProject = Trie.get(projects, key(name), Text.equal);
    switch (maybeProject) {
        case (?project) {
            if (project.ended) {
                throw Error.reject("Project has ended");
            };
            let totalRaised = project.totalRaised + amount;
            let ended = totalRaised >= project.goal;
            let (newProjects, _) = Trie.put(projects, key(name), Text.equal, { project with totalRaised = totalRaised; ended = ended });
            projects := newProjects;
        };
        case null { throw Error.reject("Project not found") };
    };
};

// end a project make sure only the owner can end the project
    public shared(msg) func endProject(name: Text) : async () {
    let maybeProject = Trie.get(projects, key(name), Text.equal);
    switch (maybeProject) {
        case (?project) {
            if (project.owner == msg.caller) {
                let (newProjects, _) = Trie.put(projects, key(name), Text.equal, { project with ended = true });
                projects := newProjects;
            } else {
                throw Error.reject("Only the owner can end the project");
            };
        };
        case null { throw Error.reject("Project not found") };
    };
};

// define isAuthenicated function
func isAuthenticated(principal: Principal) : Bool {
    let iter = Iter.fromArray([1, 2, 3]);
    let maybeNextValue = iter.next();
    switch (maybeNextValue) {
        case (?nextValue) true;
        case null false;
    };
}
};