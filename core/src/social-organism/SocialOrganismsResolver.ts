import { Arg, Mutation, Resolver } from "type-graphql";
import { LanguageRef } from "../language/LanguageRef";
import { NeighbourhoodExpression } from "../neighbourhood/Neighbourhood";
import { PerspectiveHandle } from "../perspectives/PerspectiveHandle";
import { PerspectiveInput } from "../perspectives/Perspective";
import { SocialOrganismInput, SocialOrganismProof } from "./SocialOrganism";

/**
 */
@Resolver()
export default class SocialOrganismsResolver {
    @Mutation(returns => String)
    socialorganismCreate(
        @Arg('socialOrgansim') socialOrganism: SocialOrganismInput,
    ): string {
        return "socialorganism://socialorganismAddress"
    }

    @Mutation(returns => String)
    socialOrganismSpeak(
        @Arg('organismURL') organismURL: string,
        @Arg('content') content: string, 
        @Arg('languageAddress') languageAddress: string
    ): string {
        return "HcX234"
    }

    @Mutation(returns => Boolean)
    socialOrganismConcur(
        @Arg('organismURL') organismURL: string,
        @Arg('expressionURL') expressionURL: string, 
    ): boolean {
        return true
    }

    @Mutation(returns => Boolean)
    socialOrganismRefute(
        @Arg('organismURL') organismURL: string,
        @Arg('expressionURL') expressionURL: string, 
    ): boolean {
        return true
    }

    @Mutation(returns => [String])
    socialOrganismMembers(
        @Arg('organismURL') organismURL: string,
    ): string[] {
        return ["did:ad4m:test1", "did:ad4m:test2"]
    }

    @Mutation(returns => [SocialOrganismProof])
    socialOrganismSignatures(
        @Arg('organismURL') organismURL: string,
        @Arg('expressionURL') expressionURL: string, 
    ): SocialOrganismProof[] {
        return [
            {
                expressionURL: "HcX234",
                did: "did:ad4m:test1",
                signature: "signature1",
                key: "key1"
            }
        ]
    }

    @Mutation(returns => Boolean)
    socialOrganismVouchMember(
        @Arg('organismURL') organismURL: string,
        @Arg('memberDID') memberDID: string, 
    ): boolean {
        return true
    }

    @Mutation(returns => Boolean)
    socialOrganismRefuteMember(
        @Arg('organismURL') organismURL: string,
        @Arg('memberDID') memberDID: string, 
    ): boolean {
        return true
    }
}