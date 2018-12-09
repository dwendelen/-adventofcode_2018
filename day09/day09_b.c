#include <stdio.h>
#include <stdlib.h>

struct Node {
    struct Node* prev;
    long val;
    struct Node* next;
};

typedef struct Node Node;

Node* init(long val) {
    Node* node = malloc(sizeof(Node));
    node->prev = node;
    node->val = val;
    node->next = node;

    return node;
}

Node* removeAndReturnNext(Node* node) {
    Node* prev = node->prev;
    Node* next = node->next;

    prev->next = next;
    next->prev = prev;

    free(node);
    return next;
}

Node* insertAfter(Node* node, long val) {
    Node* next = node->next;

    Node* newNode = malloc(sizeof(Node));

    node->next = newNode;
    newNode->prev = node;

    newNode->val = val;

    newNode->next = next;
    next->prev = newNode;

    return newNode;
}

#define NB_OF_PLAYERS 416

int nbOfPlayers = NB_OF_PLAYERS;
long highest = 7197500;
long scores[NB_OF_PLAYERS];



int main() {
    for(int i = 0; i < nbOfPlayers; i++) {
        scores[i] = 0;
    }

    Node* marbles = init(0);
    int player = 0;
    int marble = 1;


    while(marble <= highest) {
        if (marble % 23 == 0) {
            Node* sevenBack = marbles->prev->prev->prev->prev->prev->prev->prev;

            int scoreToAdd = sevenBack->val + marble;
            scores[player] = scores[player] + scoreToAdd;

            marbles = removeAndReturnNext(sevenBack);

        } else {
            marbles = insertAfter(marbles->next, marble);
        }

        marble++;
        player = (player + 1) % nbOfPlayers;
    }


    long max = 0;
    for(int i = 0; i < nbOfPlayers; i++) {
        if(max < scores[i]) {
            max = scores[i];
        }
    }

    printf("Max score: %ld\n", max);

    return 0;
}