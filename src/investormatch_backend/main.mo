import Nat "mo:base/Nat";
import Bool "mo:base/Bool";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Hash "mo:base/Hash";
import Trie "mo:base/Trie";
import Error "mo:base/Error";
import Iter "mo:base/Iter";
import TrieMap "mo:base/TrieMap";
import Array "mo:base/Array";
import Nat8 "mo:base/Nat8";
import Blob "mo:base/Blob";



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
	};

	// define a User type for the user's details
	// more details can be added as required
	type User = {
		email: Text;
		firstName: Text;
		lastName: Text;
		password: Nat;
	};

	// Custom SHA-256 hashing function
	// should look into/consider a more robust hashing function
	func sha256(input: Text) : Nat {
		let blob = Text.encodeUtf8(input);
		let array = Blob.toArray(blob);
		
		// Helper function for recursion
		func sha256Helper(index: Nat, currentResult: Nat) : Nat {
			if (index >= Array.size(array)) {
				return currentResult;
			} else {
				let subArrayIterator = Array.slice(array, index, 1);
				let subArray = Iter.toArray(subArrayIterator);
				let byte = Array.size(subArray);
				let updatedResult = currentResult * 256 + byte;
				return sha256Helper(index + 1, updatedResult);
			};
		};

		// Start recursion from index 0
		return sha256Helper(0, 0);
	};


	var users: TrieMap.TrieMap<Text, User> = TrieMap.TrieMap<Text, User>(Text.equal, Text.hash);

	public func registerUser(email: Text, firstName: Text, lastName: Text, password: Text): async Bool {
		let user: User = {
			email = email;
			firstName = firstName;
			lastName = lastName;
			password = sha256(password);
		};
		let existingUser = users.get(email);
		if (existingUser == null) {
			users.put(email, user);
			// bool to be handled by frontend js
			return true;
		} else {
			// bool to be handled by frontend js
			return false;
		};
	};

	public func loginUser(email: Text, password: Text) : async Text {
		let existingUser = users.get(email);

		let hashedPassword = sha256(password);

        switch(existingUser) {
            case(null) {
                return "Invalid Login Details";
            };
            case(?user) {
				if (email == user.email and user.password == hashedPassword) {
					// current return statements are for testing purposes
					// add login logic
                    // a bool that directs js on how to proceed
                    return "Logged in";
                } else {
                    return "Invalid Login Details"; 
                };
            };
        };
	};

	// a function purely for testing purposes
	public query func getUser(email: Text) : async Text {
        let existingUser = users.get(email);

        switch (existingUser) {
            case (null) {
                return "No Account Associated with that Email";
            };
            case (?user) {
                return "User: " # user.firstName;
            };
        };

    };

};